// Code for cleaning and working with ASCII guitar tab files, e.g. from OLGA.

#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>
#include <unordered_set>

#include "util.h"
#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "edit-distance.h"

#include "headers.h"
#include "guitarchive.h"

using namespace std;

static constexpr bool DRY_RUN = false;

// Can improve duplicate artist/title/album heuristics,
// or perhaps just force taking the first one now
// files that have two copies of the same tab in 'em

// bytes 3 Jan 2020: 1829958389
// bytes 4 Jan 2020: 1824308619
// bytes 8 Jan 2020: 1823968001
// bytes 10 Jan 2020: 1823938080
// bytes 11 Jan 2020: 1823238643
// bytes 12 Jan 2020: 1823206228
// bytes 18 Jan 2020: 1822694213
// bytes 27 Jan 2020: 1822238513
// bytes 1 Feb 2020:  1822182696
// bytes 7 Feb 2020:  1808539054

// metadata 8 Jan 2020: 125276
// metadata 10 Jan 2020: 129047
// metadata 11 Jan 2020: 129738
// metadata 12 Jan 2020: 130880
// metadata 18 Jan 2020: 134800
// metadata 27 Jan 2020: 136237
// metadata 7 Feb 2020: 136907

// TODO: untabify
// TODO: Remove duplicate files!
// TODO: Remove date header
// TODO: remove various boring email headers
// TODO: Title or Artist field matches "from the album ..."
// TODO: Strip (bass), (guitar), (intro) etc. from title/artist
// " From the 1993 Album "The Sign""

static constexpr const char *DIRS[] = {
  "c:\\code\\electron-guitar\\tabscrape\\tabs",
  "d:\\temp\\olga",
  "d:\\temp\\tabs",
};

static RE2::Options MultiLine() {
  RE2::Options opt;
  opt.set_posix_syntax(true);
  opt.set_one_line(false);
  return opt;
}


struct Parser {
  bool GetMetadata(const string &filename,
		   const string &contents,
		   string *title,
		   string *artist) const {
    re2::StringPiece cont(contents);
    if (RE2::Consume(&cont, explicit_meta, title, artist)) {
      return true;
    }

    // XXXX
    return false;
  }

  string FixMetadata(const string &filename, string contents) const {
    RE2::GlobalReplace(&contents, bandspace,
		       "Title: \\3\n"
		       "Artist: \\1\n"
		       "Album: \\2\n");
    RE2::GlobalReplace(&contents, songtitle, "Title: \\1\n");
    RE2::GlobalReplace(&contents, originalalbum, "Album: \\1\n");
    RE2::GlobalReplace(&contents, fromthealbum, "Album: \\1\n");
    RE2::GlobalReplace(&contents, authorartist, "Artist: \\1\n");

    string title, artist;
    if (Extract1(filename, &contents, &title, &artist) ||
	Extract2(filename, &contents, &title, &artist) ||
	Extract3(filename, &contents, &title, &artist)) {
      contents = StringPrintf("Title: %s\n"
			      "Artist: %s\n"
			      "%s",
			      title.c_str(),
			      artist.c_str(),
			      contents.c_str());
    }
    return contents;
  }

  bool ExtractFile(const string &filename,
		   string *title, string *artist) const {
    // Note: Artist, then title.
    if (!RE2::FullMatch(filename, tabfilename, artist, title) &&
	!RE2::FullMatch(filename, olgafilename, artist, title))
      return false;
    if (*artist == "misc_unsigned_bands" || *artist == "misc_your_songs" ||
	*artist == "unknown" || *artist == "various_artists") {
      return false;
    }

    // remove _ver3 etc.
    RE2::GlobalReplace(title, remove_ver, "");
    return true;
  }

  bool Extract1(const string &filename, string *contents,
	       string *title, string *artist) const {
    re2::StringPiece cont(*contents);
    string h1, h2;
    if (RE2::Consume(&cont, dashedheader, &h1, title, artist, &h2)) {
      if (EditDistance::Ukkonen(h1, h2, 5) > 3) return false;
      *title = Util::LoseWhiteR(*title);
      *artist = Util::LoseWhiteR(*artist);
      string ltitle = Util::lcase(*title);
      string lartist = Util::lcase(*artist);
      if (ltitle.find("http:") != string::npos) return false;
      if (lartist.find("http:") != string::npos) return false;
      if (ltitle.find(" - ") != string::npos) return false;
      if (lartist.find(" - ") != string::npos) return false;

      if (ltitle == "band name" || ltitle == "song name") return false;
      if (lartist == "band name" || lartist == "song name") return false;

      // Require corroboration with filenames.
      // Both artist - title and title - artist appear in real data, alas.
      string ftitle, fartist;
      if (ExtractLFile(filename, &ftitle, &fartist)) {
	int same_loss = EditDistance::Distance(ftitle, ltitle) +
	  EditDistance::Distance(fartist, lartist);
	int diff_loss = EditDistance::Distance(ftitle, lartist) +
	  EditDistance::Distance(fartist, ltitle);
	if (diff_loss < same_loss) {
	  title->swap(*artist);
	}
	// XXX thresholds?

	*contents = cont.as_string();
	return true;
      }
    }
    return false;
  }

  bool Extract2(const string &filename, string *contents,
		string *title, string *artist) const {
    re2::StringPiece cont(*contents);
    if (RE2::Consume(&cont, byline, title, artist) ||
	RE2::Consume(&cont, quotedmultiby, title, artist)) {
      *title = Util::LoseWhiteR(*title);
      *artist = Util::LoseWhiteR(*artist);

      RE2::GlobalReplace(title, "[- ,\t]*$", "");
      RE2::GlobalReplace(artist, "[- ,\t]*$", "");

      if (RE2::FullMatch(*title, ".* [Tt]ab$"))
	return false;
      if (RE2::FullMatch(*artist, ".* [Tt]ab$"))
	return false;

      if (RE2::FullMatch(*title, "^Tab:.*"))
	return false;
      if (RE2::FullMatch(*title, "^Song[ :].*"))
	return false;

      string ltitle = Util::lcase(*title);
      string lartist = Util::lcase(*artist);
      if (ltitle.find("http:") != string::npos) return false;
      if (lartist.find("http:") != string::npos) return false;
      if (ltitle.find(" - ") != string::npos) return false;
      if (lartist.find(" - ") != string::npos) return false;

      if (ltitle == "band name" || ltitle == "song name") return false;
      if (lartist == "band name" || lartist == "song name") return false;

      // Require corroboration with filenames.
      // Here since we have "title by artist", it should really match that way.
      string ftitle, fartist;
      if (ExtractFile(filename, &ftitle, &fartist)) {
	auto ReplaceUnder = [](string s) {
	    for (char &c : s) {
	      if (c == '_') c = ' ';
	    }
	    return s;
	  };
	ftitle = ReplaceUnder(Util::lcase(ftitle));
	fartist = ReplaceUnder(Util::lcase(fartist));

	int same_loss = EditDistance::Distance(ftitle, ltitle) +
	  EditDistance::Distance(fartist, lartist);
	int diff_loss = EditDistance::Distance(ftitle, lartist) +
	  EditDistance::Distance(fartist, ltitle);
	if (diff_loss < same_loss) {
	  return false;
	}

	if (same_loss > 10)
	  return false;

	*contents = cont.as_string();
	return true;
      }
    }
    return false;
  }

  bool Extract3(const string &filename, string *contents,
		string *title, string *artist) const {
    re2::StringPiece cont(*contents);
    if (RE2::Consume(&cont, dashline, title, artist)) {
      *title = Util::LoseWhiteR(*title);
      *artist = Util::LoseWhiteR(*artist);

      RE2::GlobalReplace(title, "[- ,|\t]*$", "");
      RE2::GlobalReplace(artist, "[- ,|\t]*$", "");

      if (RE2::FullMatch(*title, ".* (?:[Bb]ass )?[Tt]ab$"))
	return false;
      if (RE2::FullMatch(*artist, ".* (?:[Bb]ass )?[Tt]ab$"))
	return false;

      if (RE2::FullMatch(*title, "^(?:[Bb]ass )Tab:.*"))
	return false;
      if (RE2::FullMatch(*title, "^Song[ :].*"))
	return false;

      string ltitle = Util::lcase(*title);
      string lartist = Util::lcase(*artist);
      if (ltitle.find("http:") != string::npos) return false;
      if (lartist.find("http:") != string::npos) return false;
      if (ltitle.find(" - ") != string::npos) return false;
      if (lartist.find(" - ") != string::npos) return false;

      // Field still has placeholder, or else "BAND - PINK FLOYD"
      for (const char *no : {"band", "song", "band name", "song name",
	    "artist", "title", "track"}) {
	if (ltitle == no || lartist == no) return false;
      }

      // Require corroboration with filenames.
      // Here since we have "title by artist", it should really match that way.
      string ftitle, fartist;
      if (ExtractLFile(filename, &ftitle, &fartist)) {

	int same_loss = EditDistance::Distance(ftitle, ltitle) +
	  EditDistance::Distance(fartist, lartist);
	int diff_loss = EditDistance::Distance(ftitle, lartist) +
	  EditDistance::Distance(fartist, ltitle);
	if (diff_loss < same_loss) {
	  title->swap(*artist);
	}
	if (std::min(same_loss, diff_loss) > 10)
	  return false;

	*contents = cont.as_string();
	return true;
      }
    }
    return false;
  }

  bool ExtractLFile(const string &filename,
		    string *ftitle, string *fartist) const {
    if (ExtractFile(filename, ftitle, fartist)) {
      auto ReplaceUnder = [](string s) {
	  for (char &c : s) {
	    if (c == '_') c = ' ';
	  }
	  return s;
	};
      *ftitle = ReplaceUnder(Util::lcase(*ftitle));
      *fartist = ReplaceUnder(Util::lcase(*fartist));
      return true;
    }
    return false;
  }

  // Extract Key: Value headers starting the file.
  vector<pair<string, string>> ExtractHeaders(string *contents) const {
    vector<pair<string, string>> hdrs;
    re2::StringPiece cont(*contents);

    do {
      re2::StringPiece tmp = cont;
      string key, value;

      // Note: Be careful about cases where an earlier RE matches and so
      // a later more-specific one cannot.
      bool ok = RE2::Consume(&tmp, splitheader, &key, &value);
      ok = ok || RE2::Consume(&tmp, mischeader, &key, &value);

      if (!ok) {
	if (RE2::Consume(&tmp, standardtuning)) {
	  key = "Tuning";
	  value = "Standard";
	  ok = true;
	}
      }

      if (!ok) {
	if (RE2::Consume(&tmp, tabbedby, &value) ||
	    RE2::Consume(&tmp, transcribedby, &value)) {
	  key = "Tabbed by";
	  ok = true;
	}
      }

      ok = ok || (RE2::Consume(&tmp, knownheader, &key, &value) &&
		  value.find(" - ") == string::npos);

      if (!ok) break;

      key = Util::LoseWhiteR(key);
      value = Util::LoseWhiteR(value);

      auto OkEmpty = [](const string &lk) {
	  return lk == "subject";
	};

      string lkey = Util::lcase(key);
      int dashes = 0;
      for (char c : value) if (c == '-') dashes++;
      if (dashes > 6 ||
	  (value.empty() && !OkEmpty(lkey)) ||
	  lkey == "http" ||
	  (lkey.size() == 1 &&
	   lkey[0] >= 'a' && lkey[0] < 'g') || // note/chord names
	  lkey == "am" ||
	  lkey == "00" || lkey == "0" ||
	  lkey.find("riff") == 0 ||
	  (lkey.find("chords") == 0 && lkey != "chords by") ||
	  lkey.find("chorus") == 0 ||
	  lkey.find("intro") == 0 ||
	  lkey.find("define") == 0 ||
	  lkey.find("guitar") == 0 ||
	  lkey.find("verse") == 0 ||
	  lkey.find("note") == 0) {
	// These are not part of the headers, but rather introduce the
	// content. Stop processing, and don't commit the 'consume'
	// since it deletes leading whitespace.
	break;
      }

      hdrs.emplace_back(key, value);
      cont = tmp;
    } while (true);

    *contents = cont.as_string();
    return hdrs;
  }

  bool FilterHeaders(vector<pair<string, string>> *hdrs) const {
    std::unordered_map<string, string> already;
    vector<pair<string, string>> ret;
    ret.reserve(hdrs->size());
    bool changed = false;
    for (auto &p : *hdrs) {
      // Skip exact duplicates.
      auto it = already.find(p.first);
      if (it != already.end()) {
	const string &prev = it->second;
	const string &now = p.second;

	// Skip exact duplicates.
	if (prev == now) {
	  changed = true;
	  continue;
	}
      }
      already[p.first] = p.second;

      string lkey = Util::lcase(p.first);
      if (lkey == "content-transfer-encoding" ||
	  lkey == "message-id" ||
	  lkey == "mime-version" ||
	  lkey == "content-length" ||
	  lkey == "content-type" ||
	  lkey == "x-newsgroup-to-support" ||
	  lkey == "x-mailer" ||
	  lkey == "x-pop3-rcpt") {
	changed = true;
	continue;
      }
      if (lkey == "received" &&
	  p.second.find("SMTP") != string::npos) {
	changed = true;
	// p.first += " [DELETE]";
	continue;
      }
      ret.push_back(std::move(p));
    }
    ret.swap(*hdrs);
    return changed;
  }

  void DeduplicateHeaders(const string &filename,
			  vector<pair<string, string>> *hdrs) const {
    // Keys.
    std::vector<string> original_order;
    std::unordered_map<string, vector<string>> collated;
    for (const auto &p : *hdrs) {
      vector<string> *vals = &collated[p.first];
      // First time we see it, add to original order.
      if (vals->empty())
	original_order.push_back(p.first);
      vals->push_back(p.second);
    }

    // Larger scores better.
    auto ScoreString = [](const string &s) -> double {
	bool has_lcase = false, has_ucase = false;
	bool has_uscore = false;
	int parens = 0;
	int good_punc = 0;
	for (char c : s) {
	  if (c >= 'a' && c <= 'z') has_lcase = true;
	  if (c >= 'A' && c <= 'Z') has_ucase = true;
	  if (c == '_') has_uscore = true;
	  if (c == '(' || c == ')') parens++;
	  if (c == '\'' || c == ',' || c == '-') good_punc++;
	}

	double by_score = s.find(" by ") != string::npos ? -10.0 : 0.0;
	double from_score = s.find(" from ") != string::npos ? -10.0 : 0.0;
	double http_score = s.find("http:") != string::npos ? -40.0 : 0.0;
	double paren_score = -parens;

	double punc_score = (good_punc > 0) ? 2.0 : 0.0;

	double case_score = (has_lcase && has_ucase) ? 10.0 : 0.0;
	double underscore_score = has_uscore ? -5.0 : 0.0;
	return by_score + from_score + paren_score + http_score +
	  punc_score +
	  case_score + underscore_score;
      };

    auto StripParenthetical = [](string *s) {
	string rest;
	if (RE2::FullMatch(*s, "(.*)\\([^()]+\\)", &rest)) {
	  *s = rest;
	}
      };

    // Try to make sure that there is just one of these key fields.
    for (const string &key : original_order) {
      if (key == "Title" || key == "Artist" || key == "Album") {
	vector<string> *vals = &collated[key];
	if (vals->size() > 1) {
	  string v1 = (*vals)[0];
	  string v2 = (*vals)[1];

	  string lv1 = Util::lcase(v1);
	  string lv2 = Util::lcase(v2);
	  static const char *strip_chars = "[-,_'\\.\\\"]";
	  RE2::GlobalReplace(&lv1, strip_chars, "");
	  RE2::GlobalReplace(&lv2, strip_chars, "");
	  StripParenthetical(&lv1);
	  StripParenthetical(&lv2);


	  int loss = EditDistance::Distance(lv1, lv2);
	  int mlen = std::min(lv1.size(), lv2.size());
	  if (loss <= (int)(mlen / 3)) {
	    // They are very similar, so pick the one that
	    // we think looks better.
	    double s1 = ScoreString(v1);
	    double s2 = ScoreString(v2);

	    // XXX this only reconciles the first two...
	    vector<string> new_vals;
	    if (s1 >= s2) new_vals.push_back(v1);
	    else new_vals.push_back(v2);
	    for (int i = 2; i < (int)vals->size(); i++)
	      new_vals.push_back((*vals)[i]);
	    vals->swap(new_vals);
	  }
	}
      }
    }

    vector<pair<string, string>> ret;
    for (const string &key : original_order) {
      for (const string &value : collated[key]) {
	ret.emplace_back(key, value);
      }
    }
    ret.swap(*hdrs);
  }

  // Puts the headers in the canonical Title/Artist/Album order if possible,
  // but otherwise leaves them intact. Must be exactly capital-t Title, etc.
  void SortHeaders(vector<pair<string, string>> *hdrs) const {
    std::stable_sort(hdrs->begin(),
		     hdrs->end(),
		     [](const pair<string, string> &a,
			const pair<string, string> &b) {
		       const string &keya = a.first;
		       const string &keyb = b.first;
		       if (keya == keyb)
			 return false;

		       if (keya == "Title") return true;
		       if (keyb == "Title") return false;
		       if (keya == "Artist") return true;
		       if (keyb == "Artist") return false;
		       if (keya == "Album") return true;
		       if (keyb == "Album") return false;

		       // Otherwise, keep same order.
		       return false;
		     });
  }

  RE2 cleanvalue1{"<(.+)>"};
  RE2 cleanvalue2{"\"([^\"]+)\""};
  RE2 cleanvalue3{"'(.+)'"};
  RE2 cleanvalue4{"(.+),"};

  void NormalizeHeaders(const string &filename,
			vector<pair<string, string>> *hdrs) const {
    vector<pair<string, string>> addme;
    auto Clean = [this](const string &v, bool parens) {
	string ret;
	if (RE2::FullMatch(v, cleanvalue1, &ret))
	  return ret;
	if (RE2::FullMatch(v, cleanvalue2, &ret))
	  return ret;
	if (RE2::FullMatch(v, cleanvalue3, &ret))
	  return ret;
	if (RE2::FullMatch(v, cleanvalue4, &ret))
	  return ret;

	if (parens && v.size() > 2 && v[0] == '(' && v[v.size() - 1] == ')')
	  return v.substr(1, v.size() - 2);

	return v;
      };

    for (pair<string, string> &p : *hdrs) {
      const string lkey = Util::lcase(p.first);
      if (lkey == "title" || lkey == "song" || lkey == "songtitle" ||
	  lkey == "titolo" || lkey == "track title" || lkey == "titile" ||
	  lkey == "itle" || lkey == "track name" || lkey == "song tittle" ||
	  lkey == "track title" || lkey == "tilte" || lkey == "titel" ||
	  lkey == "titre" || lkey == "song title" || lkey == "song name" ||
	  lkey == "tittel" ||
	  lkey == "tittle" || lkey == "titulo" || lkey == "-title"
	  ) {
	p.first = "Title";
	p.second = Clean(p.second, true);

	string cleaned, album;
	if (RE2::FullMatch(p.second, "(.+)[ \t]+from +the +album[ \t]+(.+)",
			   &cleaned, &album)) {
	  addme.emplace_back("Album", album);
	  p.second = cleaned;
	}

      } else if (lkey == "artist" || lkey == "band" ||
		 lkey == "artist(s)" ||
		 lkey == "band name" || lkey == "group" ||
		 lkey == "artists" || lkey == "arist" ||
		 lkey == "band/performed by" || lkey == "autor" ||
		 lkey == "band/artist" || lkey == "artist/band" ||
		 lkey == "artiste" || lkey == "band / artist" ||
		 lkey == "artist/group" || lkey == "aritst" ||
		 lkey == "artist name" || lkey == "artis" ||
		 lkey == "song artist" || lkey == "auther/artist" ||
		 lkey == "author/band" || lkey == "artista" ||
		 lkey == "artisit" || lkey == "artsit" ||
		 lkey == "artist(s)" ||
		 lkey == "artsist" || lkey == "autore" ||
		 lkey == "banda" || lkey == "atrist" ||
		 lkey == "band/singer" || lkey == "original artist" ||
		 lkey == "artiest" || lkey == "groupe" ||
		 lkey == "rtist" || lkey =="artist/ band") {
	p.first = "Artist";
	p.second = Clean(p.second, true);

	string cleaned, album;
	if (RE2::FullMatch(p.second, "(.+)[ \t]+from +the +album[ \t]+(.+)",
			   &cleaned, &album)) {
	  addme.emplace_back("Album", album);
	  p.second = cleaned;
	}

      } else if (lkey == "album" || lkey == "albm" ||
		 lkey == "ablum" || lkey == "albumn" ||
		 lkey == "alblum" || lkey == "albulm" ||
		 lkey == "cd" || lkey == "record" ||
		 lkey == "album name" || lkey == "allbum" ||
		 lkey == "album/ep" || lkey == "-album" ||
		 lkey == "taken from the album" ||
		 lkey == "from the album" ||

		 // Release includes years.
		 // Taken from includes stuff like http://youtube...
		 // lkey == "release" || lkey == "taken from" ||

		 lkey == "album title" || lkey == "from album") {
	p.first = "Album";
	p.second = Clean(p.second, false);

	string cleaned, year;
	if (RE2::FullMatch(p.second, "(.+)[ \t]+\\(((?:19|20)[0-9][0-9])\\)",
			   &cleaned, &year)) {
	  addme.emplace_back("Year", year);
	  p.second = cleaned;
	}
      } else if (lkey == "tuning" || lkey == "tunning" ||
		 lkey == "tuneing" || lkey == "tuned" ||
		 lkey == "tunnig" || lkey == "tunig" ||
		 lkey == "tunging" || lkey == "tune") {
	p.first = "Tuning";
      } else if (lkey == "capo") {
	p.first = "Capo";
      } else if (lkey == "tabbed by" || lkey == "tabbedby" ||
		 lkey == "tabed by" || lkey == "tabber" ||
		 lkey == "taber" || lkey == "tabbe by" ||
		 lkey == "tab by" || lkey == "tabs by" ||
		 lkey == "tabledited by" ||
		 lkey == "tab author" || lkey == "tabbed out by" ||
		 lkey == "transcribe by" || lkey == "original tab by" ||
		 lkey == "tabbeb by" || lkey == "transcibed by" ||
		 lkey == "transcripted by" ||
		 lkey == "transcribed by" || lkey == "transcriber") {
	p.first = "Tabbed by";

	string email, name;
	// Rewrite both:
	//   name@example.com (My Name)
	//   My Name (name@example.com)
	if (RE2::FullMatch(p.second,
			   "([^ ]+@[^ ]+\\.[^ ]+)[ \t]+\\(([A-Z][a-z]+ [A-Z][a-z]+)\\)\\.? *",
			   &email, &name)) {
	  p.first = "E-mail";
	  p.second = email;

	  addme.emplace_back("Tabbed by", name);
	} else if (RE2::FullMatch(
		       p.second,
		       "([A-Z][a-z]+ [A-Z][a-z]+)[ \t]+\\(([^ ]+@[^ ]+\\.[^ ]+)\\)\\.? *",
		       &name, &email)) {
	  p.second = name;
	  addme.emplace_back("E-mail", email);
	}

      } else if (lkey == "email" || lkey == "e-mail" || lkey == "mail" ||
		 lkey == "email address" || lkey == "e-mail address" ||
		 lkey == "tabbers email" || lkey == "my email" ||
		 lkey == "email add" || lkey == "emil" ||
		 lkey == "contact email") {
	p.first = "E-mail";
      } else if (lkey == "date") {
	p.first = "Date";
      } else if (lkey == "key") {
	p.first = "Key";
      } else if (lkey == "year") {
	p.first = "Year";
      } else if (lkey == "by" ||
		 lkey == "written by" ||
		 lkey == "performed by" ||
		 lkey == "composer") {
	// These need corroboration
	string ftitle, fartist;
	if (ExtractLFile(filename, &ftitle, &fartist)) {
	  string lvalue = Util::lcase(p.second);
	  int loss = EditDistance::Distance(lvalue, fartist);
	  if (loss <= (int)(lvalue.size() / 3)) {
	    p.first = "Artist";
	  }
	}
      } else if (lkey == "tab") {
	string artist, title;
	// Note: just finding the word "by" picks up a lot of
	// stray stuff.
	if (RE2::FullMatch(p.second, " *(.+) +[Bb][Yy] +(.+)",
			   &title, &artist)) {
	  addme.emplace_back("Artist", artist);
	  p.first = "Title";
	  p.second = title;
	} else {
	  // p.first = "Tab";
	}
      } else if (lkey == "subject") {
	string artist, title;
	if (RE2::FullMatch(p.second,
			   " *(?:[Bb]?[Tt][Aa][Bb]|[Cc][Rr][Dd]):"
			   " *(.+) +[Bb][Yy] +(.+)",
			   &title, &artist)) {
	  addme.emplace_back("Artist", artist);
	  p.first = "Title";
	  p.second = title;
	} else {
	  p.first = "Subject";
	}
      }
      p.second = Util::losewhitel(p.second);
      p.second = Util::LoseWhiteR(p.second);
    }

    for (const auto &p : addme) {
      hdrs->push_back(p);
    }
  }

  string FixHeaders(const string &filename,
		    string contents) const {
    string out;
    out.reserve(contents.size());

    vector<pair<string, string>> hdrs = ExtractHeaders(&contents);
    FilterHeaders(&hdrs);
    NormalizeHeaders(filename, &hdrs);
    DeduplicateHeaders(filename, &hdrs);
    SortHeaders(&hdrs);
    for (const auto &p : hdrs) {
      out += StringPrintf("%s: %s\n", p.first.c_str(), p.second.c_str());
    }
    out += contents;
    return out;
  }

# define HEADER_BLANKLINES "(?: *[-=*#_]*\n)*"

  RE2 mischeader{
    // Allow totally blank lines.
    // HEADER_BLANKLINES
    "\n*"
    " *([-A-Za-z0-9'/ ]+) *: *(.*)\n"};

  // Commonly spills.
  RE2 splitheader{"\n*"
		  // Don't care about preserving the two lines here,
		  // since we discard this header anyway.
                  "(Received): ((?:from|by) .+)\n"
		  "[ \t]+.+\n"};

  RE2 standardtuning{HEADER_BLANKLINES
		     "(?i)[ \t]*standard +tuning\n"};

  RE2 tabbedby{HEADER_BLANKLINES
	       "(?i)[ \t]*tabbed +by +(.+)\n"};

  RE2 transcribedby{HEADER_BLANKLINES
		    "(?i)[ \t]*transcri(?:pt|b)ed +by +(.+)\n"};

  RE2 knownheader{
    // "[*]*\n*"
    HEADER_BLANKLINES
    "(?i)" // case insensitive
    "[ \t]*("
    "[Ss]ong|"
    "song title|"
    "[Tt]itle|"
    "[Aa]lbum|"
    "[Aa]rtist|"
    "[Aa]rtist\\(s\\)|"
    "artist/group|"
    "transcribed +by|"
    "[Bb]and|"
    "[Bb]y|"
    "[Nn]ote|"
    "[Dd]ate|"
    "[Ff]rom|"
    "[Cc]apo|"
    "[Ss]ubmitted +[Bb]y|"
    "[Tt]abbed|"
    "tabber|"
    "[Yy]ear|"
    "[Ww]ritten +[Bb]y|"
    "[Tt]ab|"
    "[Ss]ubject|"
    "[Tt]uning|"
    "[Tt]abbed +[Bb]y|"
    "[Ee]-?[Mm]ail"
    ")"
    " *[-:=]+ *(.*)\n"};


  // Preferred explicit form at the beginning of the file.
  RE2 explicit_meta{"Title: (.*)\n"
                    "Artist: (.*)\n"};
  RE2 bandspace{"Band   (.*)\n"
		"Album: (.*)\n"
		"Song: (.*)\n"};

  RE2 originalalbum{"^[Oo]riginal [Aa]lbum: *(.*)\n", MultiLine()};
  RE2 songtitle{"^[Ss]ong [Tt]itle: *(.*)\n", MultiLine()};
  RE2 fromthealbum{"^[Ff]rom [Tt]he [Aa]lbum: *(.*)\n", MultiLine()};
  RE2 authorartist{"^[Aa]uthor/[Aa]rtist *: *(.*)\n", MultiLine()};

  RE2 dashedheader{"^"
		   " *([-=_*][-=_*][-=_*][-=_*][-=_*]+)\n+"
		   "[ \t]*([A-Za-z0-9].*)[ \t]+-[ \t]+([A-Za-z0-9].*)\n+"
		   " *([-=_*][-=_*][-=_*][-=_*][-=_*]+)\n",
		   MultiLine()};
  RE2 byline{"^[ \t]*([\"']?[A-Za-z0-9].*[\"']?)[ \t]+"
	     "[Bb]y[ \t]+([A-Za-z0-9].*)\n",
	     MultiLine()};
  RE2 dashline{"^[ \t]*([A-Za-z0-9].*)[ \t]+-[ \t]+([A-Za-z0-9].*)\n",
	       MultiLine()};
  RE2 quotedmultiby{"^[ \t]*[\"']([^\"]+)[\"']\n"
		    "[ \t]*[Bb][Yy]\n"
		    "[ \t]*([A-Za-z0-9].*)\n", MultiLine()};

  RE2 remove_ver{"(?:_ver[0-9]+)?$"};

  // for "tabs" directory
  RE2 tabfilename{
    "d:\\\\temp\\\\tabs[/\\\\].[/\\\\]([^/\\\\]*)[/\\\\]([^/\\\\]*)"
    "_(?:b?tab|crd|lyr)"
    ".txt"};
  RE2 olgafilename{
    "d:\\\\temp\\\\olga[/\\\\].[/\\\\]([^/\\\\]+)[/\\\\]([^/\\\\]+)"
    "\\.[a-z]+"};
};


void FindDuplicates(const vector<pair<string, string>> &db) {
  // Each file can be identified by is index in the database.
  // Compute all the non-degenerate lines that occur in every file.
  // We'll only test for duplicates (and perhaps similarity later)
  // if the files share a line exactly.

  const int64 start = time(nullptr);
  printf("Get all lines...\n");
  fflush(stdout);
  std::mutex m;
  std::unordered_map<string, vector<int>> all_lines;
  ParallelAppi(db,
	       [&m, &all_lines](int idx, const pair<string, string> &row) {
		 vector<string> lines = Util::SplitToLines(row.second);
		 std::unordered_set<string> lines_local;
		 for (const string &line : lines) {
		   if (!line.empty() &&
		       lines_local.find(line) == lines_local.end()) {
		     // TODO: Could normalize here, or reject other
		     // really common lines that are worth skipping.

		     {
		       MutexLock ml(&m);
		       all_lines[line].push_back(idx);
		     }
		     lines_local.insert(line);
		   }
		 }
	       }, 12);
  const int64 took = time(nullptr) - start;
  printf("[Took %lld sec.] %lld distinct lines...\n",
	 took,
	 (int64)all_lines.size());


  struct HashPair {
    size_t operator()(const std::pair<int, int> &v) const {
      return (int64)v.first * 65537LL + (int64)v.second;
    }
  };

  std::unordered_set<pair<int, int>, HashPair> to_compare;
  auto Insert = [&to_compare](int a, int b) {
      if (a == b) {
	// Should not happen, but anyway, skip.
	return;
      } else if (a < b) {
	to_compare.insert(make_pair(a, b));
      } else {
	to_compare.insert(make_pair(b, a));
      }
    };

  int64 non_singleton = 0;
  int64 huge = 0;
  for (const auto &p : all_lines) {
    const vector<int> &hits = p.second;
    if (hits.size() > 1) {
      non_singleton++;

      if (hits.size() > 20) {
	huge++;
	continue;
      }


      for (int i = 0; i < (int)hits.size(); i++) {
	const int fst = hits[i];
	for (int j = i + 1; j < (int)hits.size(); j++) {
	  const int snd = hits[j];
	  CHECK(fst != snd) << fst;
	  Insert(fst, snd);
	}
      }
    }
  }

  printf("%lld occur more than once. %lld are too common.\n"
	 "%lld pairs remain to compare!\n",
	 non_singleton, huge, (int64)to_compare.size());
  fflush(stdout);

  vector<pair<int, int>> to_compare_vec;
  to_compare_vec.reserve(to_compare.size());
  for (const auto p : to_compare) {
    CHECK(p.first != p.second) << p.first;
    to_compare_vec.emplace_back(p.first, p.second);
  }
  to_compare.clear();

  // Score based on filename.
  // Note: Make sure these comparisons are actually a total order, or
  // it is possible to delete ALL versions of a file (e.g. when rock.txt,
  // paper.txt, and scissors.txt contain the same data)!
  enum FileType {
    UNKNOWN = 0,
    NUMBERED = 1,
    OLGA_BARE = 2,
    OLGA_LETTER = 3,
    OLGA_ARTIST = 4,
    TABS_MISC = 5,
    TABS = 6,
  };

  auto Classify = [](const string &file) {
      if (RE2::FullMatch(
	      file,
	      R"(c:[\\/]code[\\/]electron-guitar[\\/]tabscrape[\\/].+)"))
	return NUMBERED;
      if (RE2::FullMatch(
	      file,
	      R"(d:[\\/]temp[/\\]olga[\\/][a-z0-9][\\/][-a-z0-9_]+[\\/].+)"))
	return OLGA_ARTIST;
      if (RE2::FullMatch(
	      file,
	      R"(d:[\\/]temp[\\/]olga[\\/][a-z0-9][\\/].+)"))
	return OLGA_LETTER;
      if (RE2::FullMatch(
	      file,
	      R"(d:[\\/]temp[\\/]olga[\\/].+)"))
	return OLGA_BARE;
      if (RE2::FullMatch(
	      file,
	      R"(d:[\\/]temp[\\/]tabs[\\/][a-z0-9][\\/][-a-z0-9_]+[\\/].+)")) {
	if (file.find("misc") != string::npos ||
	    file.find("unknown") != string::npos) {
	  return TABS_MISC;
	} else {
	  return TABS;
	}
      }
      return UNKNOWN;
    };

  enum FileScore {
    HAS_SPACE = 1,
    HAS_VERSION = 2,
    HAS_DIGIT = 3,
    CLEAN = 4,
  };

  auto Score = [](const string &file) {
      if (RE2::PartialMatch(file, " ")) return HAS_SPACE;
      if (RE2::PartialMatch(file, R"(_ver[0-9]+)")) return HAS_VERSION;
      if (RE2::PartialMatch(file, R"(_[0-9]+\.)")) return HAS_DIGIT;
      return CLEAN;
    };

  printf("%d elts in to_compare_vec\n", (int)to_compare_vec.size());
  fflush(stdout);

  const int compare_start = time(nullptr);
  vector<string> info;
  int deleted = 0;
  ParallelApp(to_compare_vec,
	      [&db, &m, &info, &Classify, &Score, &deleted](
		  std::pair<int, int> cand) {
		// auto [a, b] = p;
		const int a = cand.first;
		const int b = cand.second;
		CHECK(a != b) << a;
		const string &file_a = db[a].first;
		const string &file_b = db[b].first;
		CHECK(file_a != file_b) << file_a;
		const string &contents_a = db[a].second;
		const string &contents_b = db[b].second;


		// If files are not exactly equal, we should probably
		// be smarter here...?
		string body_a = contents_a, body_b = contents_b;
		Parser p;
		vector<pair<string, string>> hdrs_a =
		  p.ExtractHeaders(&body_a);
		vector<pair<string, string>> hdrs_b =
		  p.ExtractHeaders(&body_b);

		string hdr_artist, hdr_title;
		{
		  auto Get = [](const vector<pair<string, string>> &hdrs) {
		      string artist, title;
		      for (const auto &[key, val] : hdrs) {
			if (key == "Title") title = Util::lcase(val);
			else if (key == "Artist") artist = Util::lcase(val);
		      }
		      return make_pair(artist, title);
		    };

		  auto [artist_a, title_a] = Get(hdrs_a);
		  auto [artist_b, title_b] = Get(hdrs_b);

		  if (artist_a == artist_b)
		    hdr_artist = artist_a;
		  if (title_a == title_b)
		    hdr_title = title_a;
		}


		if (body_a == body_b) {
		  FileType type_a = Classify(file_a);
		  // if (type_a == UNKNOWN) return;
		  FileType type_b = Classify(file_b);
		  // if (type_b == UNKNOWN) return;


		  const FileScore score_a = Score(file_a);
		  const FileScore score_b = Score(file_b);
		  // FileScore score_a = CLEAN,  score_b = CLEAN;

		  bool delete_a = false, delete_b = false;
		  [&](){
		      if (type_a < type_b ||
			  (type_a == type_b && score_a < score_b)) {
			delete_a = true;
			return;
		      }

		      if (type_a > type_b ||
			  (type_a == type_b && score_a > score_b)) {
			delete_b = true;
			return;
		      }

		      if (type_a == type_b &&
			  score_a == score_b) {
			RE2 onenum{"([^0-9]+)([0-9]+)([^0-9]+)"};
			string pfx_a, pfx_b, sfx_a, sfx_b;
			int num_a, num_b;
			if (RE2::FullMatch(file_a, onenum,
					   &pfx_a, &num_a, &sfx_a) &&
			    RE2::FullMatch(file_b, onenum,
					   &pfx_b, &num_b, &sfx_b) &&
			    pfx_a == pfx_b &&
			    sfx_a == sfx_b) {
			  // They just differ in one number, like ver1 and ver2.
			  // Choose the smaller one.
			  CHECK(num_a != num_b) << pfx_a << " " << pfx_b << "\n"
						<< num_a << " " << num_b << "\n"
						<< sfx_a << " " << sfx_b;
			  if (num_a < num_b) delete_b = true;
			  else delete_a = true;
			  return;
			}

			// Good artist / title match.
			string ftitle_a, fartist_a, ftitle_b, fartist_b;
			if (p.ExtractLFile(file_a, &ftitle_a, &fartist_a) &&
			    p.ExtractLFile(file_b, &ftitle_b, &fartist_b) &&
			    !fartist_a.empty() && !fartist_b.empty()) {

			  if (EditDistance::Ukkonen(fartist_a,
						    fartist_b, 5) < 3) {
			    // Same artist. Which title is better?
			    if (!hdr_title.empty()) {
			      int da = EditDistance::Distance(ftitle_a,
							      hdr_title);
			      int db = EditDistance::Distance(ftitle_b,
							      hdr_title);
			      if (da < db) {
				delete_b = true;
			      } else {
				delete_a = true;
			      }
			      return;
			    }
			  }

			  if (EditDistance::Ukkonen(ftitle_a,
						    ftitle_b, 8) < 6) {
			    // Same-ish title. Which artist is better?
			    if (!hdr_artist.empty()) {
			      int da = EditDistance::Distance(fartist_a,
							      hdr_artist);
			      int db = EditDistance::Distance(fartist_b,
							      hdr_artist);
			      if (da < db) {
				delete_b = true;
			      } else {
				delete_a = true;
			      }
			      return;
			    }
			  }
			}

			// Filenames are very close anyway...
			int dist = EditDistance::Ukkonen(file_a, file_b, 6);
			if (dist <= 5) {
			  // Just pick one arbitrarily if they are very close.
			  // (Again, be careful to have a total order!)
			  if (a < b) delete_a = true;
			  else delete_b = true;
			  return;
			}
		      }
		    }();

		  CHECK(!(delete_a && delete_b));

		  auto TryRemove = [&m, &deleted](const string &f) {
		      string ff = Util::Replace(f, "/", "\\");
		      if (remove(ff.c_str()) == 0) {
			MutexLock ml(&m);
			deleted++;
		      } else {
			MutexLock ml(&m);
			printf("Unable to remove [%s]; this can happen if "
			       "it was already removed for another reason!\n",
			       ff.c_str());
		      }
		    };

		  if (!DRY_RUN) {
		    if (delete_a) {
		      TryRemove(file_a);
		    }

		    if (delete_b) {
		      TryRemove(file_b);
		    }
		  }

		  if (true || delete_a || delete_b) {
		    MutexLock ml(&m);

		    string match = "Near";
		    if (contents_a == contents_b)
		      match = "Exact";
		    if (body_a == body_b)
		      match = "Body";
		    info.push_back(
			StringPrintf("%s duplicates (%d bytes):\n"
				     "  %s%s\n"
				     "  %s%s\n",
				     match.c_str(),
				     (int)contents_a.size(),
				     file_a.c_str(),
				     delete_a ? " (DELETE)" : "",
				     file_b.c_str(),
				     delete_b ? " (DELETE)" : ""));
		  }
		}
	      },
	      16);
  {
    FILE *ff = fopen("dups.txt", "wb");
    for (const string &line : info) {
      fprintf(ff, "%s\n", line.c_str());
    }
    fclose(ff);
  }
  const int64 compare_sec = time(nullptr) - compare_start;
  printf("[%lld sec] Possible duplicates in dups.txt: %d\n",
	 compare_sec,
	 (int)info.size());
  if (deleted > 0)
    printf("Actually deleted %d files.\n", deleted);
  fflush(stdout);
}

static constexpr int SIMILAR_DISTANCE = 200;

void ComputeDistances(const vector<pair<string, string>> &contents) {
  const int n = contents.size();
  vector<int> distances(contents.size() * contents.size(), 0);
  std::mutex m;
  int total_done = 0;
  int64 start = time(nullptr);
  ParallelComp2D(contents.size(), contents.size(),
		 [&contents, n, &distances, &total_done, &m, start](int x,
								    int y) {
		   if (x < y) {
		     int dist = EditDistance::Ukkonen(contents[x].second,
						      contents[y].second,
						      SIMILAR_DISTANCE);
		     /*
		     int dist = std::abs((int)(contents[x].second.size() -
					       contents[y].second.size()));
		     */
		     /*
		     int dist = EditDistance::Distance(contents[x].second,
						       contents[y].second);
		     */

		     distances[y * n + x] = dist;
		     bool pr = false;
		     int td = 0;
		     {
		       MutexLock ml(&m);
		       total_done++;
		       if (total_done % 1000000 == 0) {
			 pr = true;
			 td = total_done;
		       }
		     }
		     if (pr) {
		       int64 secs = time(nullptr) - start;
		       printf("%d = (%.2f%%), %.2f/s\n",
			      td,
			      (td * 100.0) / (n * (n - 1) / 2.0),
			      td / (double)secs);
		       fflush(stdout);
		     }
		   }
		 },
		 42);
  printf("Done computing distances.\n");

  FILE *f = fopen("similar.txt", "wb");
  for (int y = 0; y < n; y++) {
    for (int x = 0; x < y; x++) {
      int dist = distances[y * n + x];
      if (dist < SIMILAR_DISTANCE) {
	fprintf(f,
		"Very similar (%d):\n"
		"  %s\n"
		"  %s\n",
		dist,
		contents[x].first.c_str(),
		contents[y].first.c_str());
      }
    }
  }
  fclose(f);

  fflush(stdout);
}

static string StripCR(const string &contents) {
  string ret;
  ret.reserve(contents.size());
  for (const char c : contents) {
    if (c == '\r')
      continue;
    ret += c;
  }
  return ret;
}

static int CountSpaces(const string &line) {
  for (int i = 0; i < (int)line.size(); i++) {
    if (line[i] != ' ') return i;
  }
  return (int)line.size();
}

// Remove trailing spaces from lines, and blank lines from bottom.
// End every line (including the last) with a newline.
string StripOuterWhitespace(const string &contents) {
  vector<string> lines = Util::SplitToLines(contents);
  int min_spaces = 999;

  for (string &line : lines) {
    while (!line.empty() && line[line.size() - 1] == ' ') {
      line.resize(line.size() - 1);
    }
    if (!line.empty()) {
      min_spaces = std::min(min_spaces, CountSpaces(line));
    }
  }

  // And remove empty lines at the end.
  while (!lines.empty() && lines[lines.size() - 1].empty()) {
    lines.resize(lines.size() - 1);
  }

  // ad hoc
  while (!lines.empty() &&
	 RE2::FullMatch(lines[lines.size() - 1], "_________+")) {
    lines.resize(lines.size() - 1);
  }
  
  // Now if every non-empty line starts with space, remove it.
  if (min_spaces > 0) {
    for (string &line : lines) {
      if ((int)line.size() < min_spaces) {
	line = "";
      } else {
	line = line.substr(min_spaces);
      }
    }
  }

  bool only_blank = true;
  string out;
  out.reserve(contents.size());
  for (const string &line : lines) {
    if (line.empty()) {
      if (only_blank) {
	// Skip leading blank lines.
      } else {
	out += "\n";
      }
    } else {
      out += line;
      out += "\n";
      only_blank = false;
    }
  }
  return out;
}

// Same, but not taking headers into account (and incidentally, doing
// some normalization of them). Main thing is that the body is sometimes
// indented, and this unindents it.
static string NormalizeHeaderOuterWhitespace(const Parser &p, string body) {
  vector<pair<string, string>> hdrs =
    p.ExtractHeaders(&body);

  // (old code to preserve leading blanks after headers)
  // int num_leading = 0;
  // while (num_leading < (int)body.size() && body[num_leading] == '\n')
  // num_leading++;
  // body = body.substr(num_leading, string::npos);
  body = StripOuterWhitespace(body);

  string out;
  for (const auto &[key, val] : hdrs) {
    out += StringPrintf("%s: %s\n", key.c_str(), val.c_str());
  }

  // Exactly one blank line after headers. Leading blanks were stripped
  // from body.
  if (!out.empty()) out += "\n";
  // while (num_leading--) out += "\n";

  out += body;
  return out;
}

string StripFooter(const string &contents) {
  string_view cont(contents);
  if (Guitarchive::TryStripSuffix("\nset8\n"sv, &cont)) {
    return (string)cont;
  }
  if (Guitarchive::TryStripSuffix("\nSet8\n"sv, &cont)) {
    return (string)cont;
  }
  if (Guitarchive::TryStripSuffix("\nEnjoy!\n"sv, &cont)) {
    return (string)cont;
  }
  if (Guitarchive::TryStripSuffix("\nenjoy!\n"sv, &cont)) {
    return (string)cont;
  }

  return contents;
}

string RewriteHeader(const string &contents) {
  static LazyRE2 gt1 = {
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n"
    "<html><head>\n"
    "<title>[^<]*</title>\n"
    "</head><body style='font-family: Courier New; font-size:11px'>\n"
    "<pre><h2 style='color:#000000'><a style='color:#000000' "
    "href='[^']*'>([^<]*)</a> "};

  re2::StringPiece cont(contents);
  string artist;
  if (RE2::Consume(&cont, *gt1, &artist)) {
    // Skip '227'
    cont.remove_prefix(1);
    static LazyRE2 gt2 =
      {" <a style='color:#000000' href='[^']*'>([^<]*) Tab</a></h2>\n"};
    string title;
    if (RE2::Consume(&cont, *gt2, &title)) {
      return
	StringPrintf("Title: %s\n"
		     "Artist: %s\n"
		     "%s", title.c_str(), artist.c_str(),
		     cont.ToString().c_str());
    }
  }
  return contents;
}

// Perhaps temporary rewrites.
string AdHocSubstitutions(string s) {
  /*
  static LazyRE2 chordsof = {
    "\nlyrics and chords of: (.+) by (.+)\n"
  };
  RE2::GlobalReplace(&s, *chordsof, "\nTitle: \\1\n"
		     "Artist: \\2\n");
  */

  RE2::GlobalReplace(&s,
		     "\n *Copyright \\(c\\) [12][90][0-9][0-9] by OLGA\n",
		     "\n");
  
  RE2::GlobalReplace(&s,
		     "\n>From the album: ",
		     "\nAlbum: ");

  /*
  RE2::GlobalReplace(&s,
		     "#----+Created with Taborama Tab Creator----+-----#\n",
		     "");
  */


  /*
  string rest;
  re2::StringPiece cont(s);
  if (RE2::Consume(&cont, "#-------------------------+#\n"))
    s = cont.ToString();
  */

  /*
  static LazyRE2 recordedby = {
    "[ \t]*[Aa]s recorded by (.+)\n"
  };
  RE2::GlobalReplace(&s, *recordedby, "\nArtist: \\1\n");
  */

  /*
  RE2 fullrecby{
    "^[ \t]*(.+)\n"
    "[ \t]*[Aa]s recorded by (.+)\n"
    "[ \t]*\\([Ff]rom the ([0-9]+) [Aa]lbum (.+)\\)\n",
    MultiLine()
  };
  RE2::GlobalReplace(&s, fullrecby,
		     "Title: \\1\n"
		     "Artist: \\2\n"
		     "Album: \\4\n"
		     "Year: \\3\n");
  */

  /*
  RE2 fullrecby2{
    "([ \t]*[-=]+\n)?"
    "^[ \t]*(.*[A-Za-z0-9].+)\n"
    "([ \t]*[-=]+\n)?"
    "[ \t]*[Aa]s recorded by (.+)\n"
    "[ \t]*\\(?[Ff]rom the [0-9]* ?[Aa]lbum (.+)\\)?\n",
    MultiLine()
  };
  RE2::GlobalReplace(&s, fullrecby2,
		     "Title: \\2\n"
		     "Artist: \\4\n"
		     "Album: \\5\n");
  */

  RE2 fullrecby3{
    "([ \t]*[-=]+\n)?"
    "^[ \t]*([^( \t\n].*[A-Za-z0-9].+[^) \t\n])\n"
    "([ \t]*[-=]+\n)?"
    "[ \t]*[Aa]s recorded by (.+)\n",
    MultiLine()
  };
  RE2::GlobalReplace(&s, fullrecby3,
		     "Title: \\2\n"
		     "Artist: \\4\n");
    return s;
}

string StripHeader(const string &contents) {
  for (const char *hdr : Headers::headers) {
    auto pos = contents.find(hdr);
    if (pos != string::npos) {
      string ret = contents.substr(0, pos);
      ret += contents.substr(pos + string(hdr).size(), string::npos);
      return ret;
    }
  }
  return contents;
}

bool HasNonAscii(const string &s) {
  for (const char c : s) {
    switch (c) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      // ok.
      break;
    default:
      if (c < ' ' || c > 127)
	return true;
    }
  }
  return false;
}

// Bad encodings. This appears to come from when windows-1252 encoded files get
// "converted" to UTF-8 with a sloppy process that just encodes e.g. 0x96 as the
// unicode U+0096, which is not a legal codepoint.
//
// There are also cases where these bytes are left bare (0x97 on its own can be
// replaced with -, etc.) but we can't just replace that byte because it may
// appear in a valid utf-8 encoding (i.e. 0xC3 0x97 is the multiply sign).
// Pretty challenging!
//
// 0xa0 is nbsp in windows-1252, which should be space. But be careful of
// the issue above!
string FixBadEncoding(const string &s) {
  string r = Util::Replace(s, "\xc2\x91", "'");
  r = Util::Replace(r, "\xc2\x92", "'");
  r = Util::Replace(r, "\xc2\x93", "\"");
  r = Util::Replace(r, "\xc2\x94", "\"");
  // These may be en and em dash. They often appear randomly in tablature,
  // almost like watermarks.
  r = Util::Replace(r, "\xc2\x96", "-");
  r = Util::Replace(r, "\xc2\x97", "-");
  // Seems to be a problem with the 'tabs' scrape.
  // r = Util::Replace(r, "\\\\\" ", "\"");
  // And for the same at EOL after trailing whitespace was removed.
  // r = Util::Replace(r, "\\\\\"\n", "\"\n");

  r = Util::Replace(r, "\xc2\xb4", "'");
  r = Util::Replace(r, "&quot;", "\"");
  // TODO: More html, &lt; &gt; &amp; are pretty common
  return r;
}

int main(int argc, char **argv) {

  vector<string> all_filenames;
  for (const char *d : DIRS) {
    Guitarchive::AddAllFilesRec(d, &all_filenames);
  }

  printf("Num files: %lld\n", (int64)all_filenames.size());

  std::unordered_set<string> filename_set;
  for (const string &f : all_filenames) {
    filename_set.insert(f);
  }

  if (false) {
    // These were only in the "b" directory, somehow?
    // filenames: 22954559 bytes
    int num_mangled = 0;
    RE2 mangled{"(d:\\\\temp\\\\tabs.*)_[0-9]+id_[0-9]+date\\.txt"};
    for (const string &f : all_filenames) {
      string part;
      if (RE2::FullMatch(f, mangled, &part)) {
	num_mangled++;
	string norm = part + ".txt";
	if (filename_set.find(norm) != filename_set.end()) {
	  printf("[!!] Has both %s and %s\n", norm.c_str(), f.c_str());
	  continue;
	}
	if (num_mangled % 100 == 0) printf("[%d] [%s] -> [%s]\n", num_mangled, f.c_str(), norm.c_str());
	// CHECK(Util::move(f, norm)) << f;
      }
    }
    printf("Num mangled: %d\n", num_mangled);
    // After moving files, we have the wrong filenames, so force exit.
    return 0;
  }

  // Remove windows file.txt -> file (2).txt disambiguation.
  if (false) {
    int num_mangled = 0;
    RE2 mangled{"(.+) \\(([0-9]+)\\)\\.(.+)"};
    for (const string &f : all_filenames) {
      int num = 0;
      string start, ext;
      if (RE2::FullMatch(f, mangled, &start, &num, &ext)) {
	num_mangled++;
	string norm = StringPrintf("%s_%d.%s", start.c_str(), num, ext.c_str());
	if (filename_set.find(norm) != filename_set.end()) {
	  printf("[!!] Has both %s and %s\n", norm.c_str(), f.c_str());
	  continue;
	}
	printf("[%d] [%s] -> [%s]\n", num_mangled, f.c_str(), norm.c_str());
	CHECK(Util::move(f, norm)) << f;
      }
    }
    printf("Num mangled: %d\n", num_mangled);
    // After moving files, we have the wrong filenames, so force exit.
    return 0;
  }


  printf("Read files..\n");
  fflush(stdout);
  vector<pair<string, string>> files =
    ParallelMap(all_filenames,
		[](const string &filename) {
		  return make_pair(filename, Util::ReadFile(filename));
		},
		16);



  if (false) {
    /*
      string bogus =
      Util::ReadFile("d:\\temp\\tabs\\a\\audioslave\\be_yourself_drum_tab.txt");
      printf("Bogus: %s\n", bogus.c_str());
      CHECK(!bogus.empty());

      for (const auto &p : files) {
      if (p.second == bogus) {
      printf("Remove bogus %s...\n", p.first.c_str());
      Util::remove(p.first);
      }
      }
      return 0;
    */
  }

  {
    int64 filename_bytes = 0;
    int64 total_bytes = 0;
    int non_ascii = 0;
    for (const auto &p : files) {
      filename_bytes += p.first.size();
      total_bytes += p.second.size();
      /*
	if ((int)p.second.size() > 100000) {
	printf("Large file [%lld]: %s\n", (int64)p.second.size(), p.first.c_str());
	}
      */
      if (HasNonAscii(p.second)) {
	non_ascii++;
	// printf("Non-ascii: %s\n", p.first.c_str());
      }
    }
    printf("Filename bytes: %lld\n", filename_bytes);
    printf("Content bytes: %lld\n", total_bytes);
    printf("Non-ascii files: %d\n", non_ascii);
    fflush(stdout);
  }

  {
    Parser p;
    std::mutex m;
    int has_marker = 0;
    vector<string> todo;
    ParallelApp(files,
		[&p, &m, &has_marker, &todo](const pair<string, string> &row) {
		  // Skip "album tabs"
		  if (row.first.find("album") != string::npos ||
		      row.first.find("compilatio") != string::npos)
		    return;


		  const string &cont = row.second;
		  /*
		    bool accept =
		    cont.find("-PLEASE NOTE-") != string::npos ||
		    cont.find("scholarship, or research") != string::npos ||
		    cont.find("Have fun, DAIRYBEAT on") != string::npos ||
		    cont.find("and has a specific punishment") != string::npos ||
		    // XXX
		    // HasNonAscii(cont) ||
		    cont.find("Song/CD") != string::npos ||
		    cont.find("in an unauthorized application,") != string::npos;
		  */

		  /*
		    string title, artist;
		    bool accept =
		    !p.GetMetadata(row.first, cont, &title, &artist);
		  */

		  /*
		  string contents = cont;
		  auto hdrs = p.ExtractHeaders(&contents);
		  bool accept = RE2::PartialMatch(contents, "(?i)\ntitle:.+\n");
		  for (const auto &[key, val] : hdrs) {
		    (void)key;
		    if (// key == "Title" &&
			val.find("====") != string::npos)
		      accept = true;
		  }
		  */
		  bool accept = RE2::PartialMatch(
		      cont,
		      "\n[=_*#][=_*#][=_*#][=_*#]+\n");

		  // p.ExtractFile(row.first, &title, &artist);


		  // bool accept =
		  // cont.find("www.Ultimate-Guitar.com") != string::npos;

		  // bool accept = cont.find("&quot;") != string::npos;

		  if (accept) {
		    MutexLock ml(&m);
		    todo.push_back(row.first);
		    if (has_marker < 20 ||
			(has_marker % 10000 == 0)) {
		      printf("Marker? %s\n", row.first.c_str());
		    }
		    has_marker++;
		  }
		},
		32);
    printf("May still have markers: %d\n", has_marker);
    FILE *ff = fopen("markers_todo.txt", "wb");
    for (const string &file : todo) {
      fprintf(ff, "%s\n", file.c_str());
    }
    fclose(ff);
    fflush(stdout);
  }


  {
    Parser p;
    std::mutex m;
    int has_meta = 0;
    ParallelApp(files,
		[&p, &m, &has_meta](const pair<string, string> &row) {
		  string title, artist;
		  if (p.GetMetadata(row.first, row.second, &title, &artist)) {
		    MutexLock ml(&m);
		    // printf("[%s] by [%s]\n", title.c_str(), artist.c_str());
		    has_meta++;
		  }
		},
		32);
    printf("Total with metadata: %d\n", has_meta);
    fflush(stdout);
  }


  if (false) {
    Parser p;
    std::mutex m;
    int has_extraction = 0;
    vector<string> examples;
    ParallelApp(files,
		[&p, &m, &has_extraction, &examples](
		    const pair<string, string> &row) {
		  string title, artist;
		  string contents = row.second;
		  if (p.Extract3(row.first, &contents, &title, &artist)) {
		    MutexLock ml(&m);
		    examples.push_back(
			StringPrintf("[%s] by [%s] (%s)\n",
				     title.c_str(), artist.c_str(),
				     row.first.c_str()));
		    has_extraction++;
		  }
		},
		32);
    printf("Total with extractions: %d\n", has_extraction);
    fflush(stdout);
    FILE *f = fopen("extractions.txt", "wb");
    for (const string &s : examples)
      fprintf(f, "%s", s.c_str());
    fclose(f);
  }

  if (false) {
    Parser p;
    std::mutex m;
    int has_extraction = 0;
    vector<string> examples;
    ParallelApp(files,
		[&p, &m, &has_extraction, &examples](
		    const pair<string, string> &row) {
		  string title, artist;
		  string contents = row.second;
		  vector<pair<string, string>> orig_hdrs =
		    p.ExtractHeaders(&contents);
		  if (!orig_hdrs.empty()) {
		    vector<pair<string, string>> hdrs = orig_hdrs;
		    p.FilterHeaders(&hdrs);
		    p.NormalizeHeaders(row.first, &hdrs);
		    p.SortHeaders(&hdrs);

		    if (orig_hdrs != hdrs) {
		      string ex = row.first + (string)"\n";
		      for (const auto &p : hdrs) {
			ex += StringPrintf("  [%s] = [%s]\n",
					   p.first.c_str(), p.second.c_str());
		      }

		      MutexLock ml(&m);
		      examples.push_back(std::move(ex));
		      has_extraction++;
		    }
		  }
		},
		32);
    printf("Total with extractions: %d\n", has_extraction);
    fflush(stdout);
    FILE *f = fopen("extractions.txt", "wb");
    for (const string &s : examples)
      fprintf(f, "%s", s.c_str());
    fclose(f);
  }

  if (true) {
    Parser p;
    std::mutex m;
    std::unordered_map<string, tuple<int, string, string>> keys;
    ParallelApp(files,
		[&p, &m, &keys](const pair<string, string> &row) {
		  string title, artist;
		  string contents = row.second;
		  vector<pair<string, string>>
		    orig_hdrs = p.ExtractHeaders(&contents);
		  if (!orig_hdrs.empty()) {
		    vector<pair<string, string>> hdrs = orig_hdrs;
		    p.FilterHeaders(&hdrs);
		    p.NormalizeHeaders(row.first, &hdrs);
		    p.SortHeaders(&hdrs);

		    MutexLock ml(&m);
		    for (const auto &kv : hdrs) {
		      auto &v = keys[kv.first];
		      std::get<0>(v)++;
		      if (std::get<1>(v).empty()) std::get<1>(v) = kv.second;
		      if (std::get<2>(v).empty()) std::get<2>(v) = row.first;
		    }
		  }
		},
		32);
    printf("Total different keys: %d\n", (int)keys.size());
    fflush(stdout);

    vector<pair<int, string>> sorted;
    sorted.reserve(keys.size());
    for (const auto &r : keys) {
      const string &key = r.first;
      int count;
      string value, file;
      std::tie(count, value, file) = r.second;
      sorted.emplace_back(count,
			  StringPrintf("[%s] = [%s] %s",
				       key.c_str(), value.c_str(),
				       file.c_str()));
    }

    std::sort(sorted.begin(), sorted.end(),
	      [](const pair<int, string> &a,
		 const pair<int, string> &b) {
		return a.first > b.first;
	      });

    FILE *f = fopen("keys.txt", "wb");
    for (const auto &r : sorted) {
      fprintf(f, "%d %s\n", r.first, r.second.c_str());
    }
    fclose(f);
  }


  if (true) {
    Parser p;
    std::mutex m;
    vector<string> previews;
    int changed = 0;
    ParallelApp(files,
		[&p, &m, &previews, &changed](const pair<string, string> &row) {
		  string c = StripOuterWhitespace(row.second);
		  c = FixBadEncoding(c);
		  c = StripCR(c);
		  c = AdHocSubstitutions(c);
		  c = StripHeader(c);
		  c = p.FixMetadata(row.first, c);
		  c = p.FixHeaders(row.first, c);
		  c = StripFooter(c);
		  c = NormalizeHeaderOuterWhitespace(p, c);

		  if (c != row.second) {
		    string filename = Guitarchive::Backslash(row.first);
		    if (DRY_RUN) {
		      static constexpr int CONTEXT = 1;
		      vector<string> s1 = Util::SplitToLines(row.second);
		      vector<string> s2 = Util::SplitToLines(c);
		      int same_begin = 0, same_end = 0;

		      int end = 0;
		      while ((s1.size() - 1) - end > 0 &&
			     (s2.size() - 1) - end > 0 &&
			     s1[(s1.size() - 1) - end] ==
			     s2[(s2.size() - 1) - end]) {
			same_end++;
			end++;
		      }
		      /*
			while (!s1.empty() && !s2.empty() &&
			s1.back() == s2.back()) {
			s1.pop_back();
			s2.pop_back();
			same_end++;
			}
		      */

		      {
			int trim_end = end - CONTEXT;
			while (trim_end > 0) {
			  s1.pop_back();
			  s2.pop_back();
			  trim_end--;
			}
		      }

		      int start = 0;
		      while (start < (int)s1.size() &&
			     start < (int)s2.size() &&
			     s1[start] == s2[start]) {
			start++;
			same_begin++;
		      }
		      if (start - CONTEXT >= 0) {
			s1.erase(s1.begin(), s1.begin() + (start - CONTEXT));
			s2.erase(s2.begin(), s2.begin() + (start - CONTEXT));
		      }



		      MutexLock ml(&m);
		      string out = StringPrintf("** %s\n"
						"(+%d before)\n",
						filename.c_str(),
						same_begin);
		      for (const string &s : s1) {
			out += s;
			out += "\n";
		      }
		      out += "->\n";
		      for (const string &s : s2) {
			out += s;
			out += "\n";
		      }
		      out += StringPrintf("(+%d after)\n", same_end);
		      previews.push_back(out);
		    } else {
		      CHECK(Util::WriteFile(filename, c)) << filename;

		      MutexLock ml(&m);
		      changed++;
		      if (changed < 10 ||
			  (changed % 1000 == 0)) {
			printf("[%d] Write [%s] (%d -> %d)\n",
			       changed,
			       filename.c_str(),
			       (int)row.second.size(), (int)c.size());
		      }
		    }
		  }
		},
		30);
    if (DRY_RUN) {
      FILE *f = fopen("dry-run.txt", "wb");
      for (const string &p : previews) {
	fprintf(f, "%s\n", p.c_str());
      }
      fclose(f);
      printf("Wrote %d changes for dry run.\n", (int)previews.size());
    } else {
      printf("Modified %d files in place.\n", changed);
    }
    fflush(stdout);
  }

  // FindDuplicates(files);

  printf("Done.\n");
  fflush(stdout);


  // ComputeDistances(files);

  return 0;
}
