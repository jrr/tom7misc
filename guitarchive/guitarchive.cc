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

using namespace std;

static constexpr bool DRY_RUN = true;

// TODO: file ends with enjoy!

// TODO duplicate title, artist, etc.
// Often these have small spelling or capitalization differences, too.

// bytes 3 Jan 2020: 1829958389
// bytes 4 Jan 2020: 1824308619

// TODO: Remove duplicates!
// TODO: Remove email header
// TODO: Remove date header

//                            As recorded by Xavier Rudd
//                          (From the 2002 Album TO LET)


static constexpr const char *DIRS[] = {
  "c:\\code\\electron-guitar\\tabscrape\\tabs",
  "d:\\temp\\olga",
  "d:\\temp\\tabs",
};

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

  static RE2::Options MultiLine() {
    RE2::Options opt;
    opt.set_posix_syntax(true);
    opt.set_one_line(false);
    return opt;
  }

  bool ExtractFile(const string &filename, string *title, string *artist) const {
    // Note: Artist, then title.
    if (!RE2::FullMatch(filename, tabfilename, artist, title))
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
      if (h1.size() != h2.size()) return false;
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

      // Require corroboration with filenames. Both artist - title and title - artist
      // appear in real data, alas.
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
    if (RE2::Consume(&cont, byline, title, artist)) {
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

  // Extract Key: Value headers starting the file.
  vector<pair<string, string>> ExtractHeaders(string *contents) const {
    vector<pair<string, string>> hdrs;
    re2::StringPiece cont(*contents);

    do {
      re2::StringPiece tmp = cont;
      string key, value;

      bool ok = RE2::Consume(&tmp, mischeader, &key, &value);

      ok = ok || (RE2::Consume(&tmp, knownheader, &key, &value) &&
		  value.find(" - ") == string::npos);

      if (!ok) break;
      
      key = Util::LoseWhiteR(key);
      value = Util::LoseWhiteR(value);

      string lkey = Util::lcase(key);
      int dashes = 0;
      for (char c : value) if (c == '-') dashes++;
      if (dashes > 6 ||
	  value.empty() ||
	  lkey == "http" ||
	  (lkey.size() == 1 && lkey[0] >= 'a' && lkey[0] < 'g') || // note/chord names
	  lkey == "am" ||
	  lkey == "00" || lkey == "0" ||
	  lkey.find("riff") == 0 ||
	  lkey.find("chords") == 0 ||
	  lkey.find("chorus") == 0 ||
	  lkey.find("intro") == 0 ||
	  lkey.find("guitar") == 0 ||
	  lkey.find("verse") == 0 ||
	  lkey.find("note") == 0) {
	// These are not part of the headers, but rather introduce the content.
	// Stop processing, and don't commit the 'consume' since it deletes leading
	// whitespace.
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
      if (it != already.end() && it->second == p.second) {
	changed = true;
	continue;
      }
      already[p.first] = p.second;
      
      string lkey = Util::lcase(p.first);
      if (lkey == "content-transfer-encoding" ||
	  lkey == "message-id") {
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

  void NormalizeHeaders(vector<pair<string, string>> *hdrs) const {
    for (pair<string, string> &p : *hdrs) {
      const string lkey = Util::lcase(p.first);
      if (lkey == "title" || lkey == "song" || lkey == "songtitle" ||
	  lkey == "titolo" || lkey == "track title" || lkey == "titile" ||
	  lkey == "itle" || lkey == "track name" || lkey == "song tittle" ||
	  lkey == "track title" || lkey == "tilte" || lkey == "titel" ||
	  lkey == "titre" || lkey == "song title" || lkey == "song name" ||
	  lkey == "tittle" || lkey == "titulo"
	  ) {
	p.first = "Title";
      } else if (lkey == "artist" || lkey == "band" ||
		 lkey == "band name" || lkey == "group" ||
		 lkey == "artists" || lkey == "arist" ||
		 lkey == "band/performed by" || lkey == "autor" ||
		 lkey == "band/artist" || lkey == "artist/band" ||
		 lkey == "artiste" || lkey == "band / artist" ||
		 lkey == "artist/group" ||
		 lkey == "artist name" || lkey == "artis" ||
		 lkey == "song artist" || lkey == "auther/artist" ||
		 lkey == "author/band" || lkey == "artista" ||
		 lkey == "artisit" || lkey == "artsit" ||
		 lkey == "artsist" || lkey == "autore" ||
		 lkey == "banda" || lkey == "atrist" ||
		 lkey == "artiest" || lkey == "groupe" ||
		 lkey == "rtist" || lkey =="artist/ band") {
	p.first = "Artist";
      } else if (lkey == "album" || lkey == "albm" ||
		 lkey == "ablum" || lkey == "albumn" ||
		 lkey == "alblum" || lkey == "albulm" ||
		 lkey == "cd" || lkey == "record" ||
		 lkey == "album name" || lkey == "allbum" ||
		 lkey == "album/ep" ||
		 lkey == "taken from the album" ||
		 lkey == "from the album" ||
		 lkey == "album title" || lkey == "from album") {
	p.first = "Album";
      } else if (lkey == "tuning" || lkey == "tunning" ||
		 lkey == "tuneing" || lkey == "tuned" ||
		 lkey == "tunging" || lkey == "tune") {
	p.first = "Tuning";
      } else if (lkey == "capo") {
	p.first = "Capo";
      } else if (lkey == "tabbed by" || lkey == "tabbedby" ||
		 lkey == "tabed by" || lkey == "tabber" ||
		 lkey == "taber" || lkey == "tabbe by" ||
		 lkey == "tab by" || lkey == "tabs by" ||
		 lkey == "tab author" || lkey == "tabbed out by" ||
		 lkey == "transcribe by" ||
		 lkey == "transcribed by" || lkey == "transcriber") {
	p.first = "Tabbed by";
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
      } else if (lkey == "subject") {
	p.first = "Subject";
      }
      p.second = Util::losewhitel(p.second);
      p.second = Util::LoseWhiteR(p.second);
    }
  }
  
  string FixHeaders(string contents) const {
    string out;
    out.reserve(contents.size());

    vector<pair<string, string>> hdrs = ExtractHeaders(&contents);
    FilterHeaders(&hdrs);
    NormalizeHeaders(&hdrs);
    SortHeaders(&hdrs);
    for (const auto &p : hdrs) {
      out += StringPrintf("%s: %s\n", p.first.c_str(), p.second.c_str());
    }
    out += contents;
    return out;
  }
  
  RE2 mischeader{
    // Allow totally blank lines.
    "\n*"
      // XXX add -
    " *([-A-Za-z0-9'/ ]+) *: *(.*)\n"};

  // XXX TODO
  RE2 knownheader{
    "\n*"
    "("
    "[Ss]ong|"
    "[Tt]itle|"
    "[Aa]lbum|"
    "[Aa]rtist|"
    "[Bb]y"
    ")"
    " *- *(.*)\n"};
  
  
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
		   "------------(-*)\n"
		   "[ \t]*([A-Za-z0-9].*)[ \t]* - [ \t]*([A-Za-z0-9].*)\n"
		   "------------(-*)\n", MultiLine()};
  RE2 byline{"^[ \t]*([A-Za-z0-9].*)[ \t]+[Bb]y[ \t]+([A-Za-z0-9].*)\n", MultiLine()};
  RE2 dashline{"^[ \t]*([A-Za-z0-9].*)[ \t]+-[ \t]+([A-Za-z0-9].*)\n", MultiLine()};
	     
  
  RE2 remove_ver{"(?:_ver[0-9]+)?$"};
  // TODO: Support OLGA dir as well
  RE2 tabfilename{"d:\\\\temp\\\\tabs[/\\\\].[/\\\\]([^/\\\\]*)[/\\\\]([^/\\\\]*)"
		  "_(?:b?tab|crd|lyr)"
		  ".txt"};
};


static void AddAllFilesRec(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (Util::isdir(filename)) {
      // printf("Dir: [%s]\n", filename.c_str());
      AddAllFilesRec(filename, all_files);
    } else {
      if (!filename.empty() &&
	  // Should perhaps just delete emacs backups..?
	  filename[filename.size() - 1] != '~') {
	all_files->push_back(filename);
      }
    }
  }
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

string StripCR(const string &contents) {
  string ret;
  ret.reserve(contents.size());
  for (const char c : contents) {
    if (c == '\r')
      continue;
    ret += c;
  }
  return ret;
}

int CountSpaces(const string &line) {
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

bool TryStripSuffix(string_view suffix,
		    string_view *s) {
  // In c++20, can use s->ends_with(suffix).
  auto EndsWith = [](string_view big, string_view little) {
      if (big.size() < little.size()) return false;
      return big.substr(big.size() - little.size()) == little;
    };
  if (EndsWith(*s, suffix)) {
    s->remove_suffix(suffix.length());
    return true;
  }
  return false;
}

string StripFooter(const string &contents) {
  string_view cont(contents);
  if (TryStripSuffix("\nset8\n"sv, &cont)) {
    return (string)cont;
  }
  if (TryStripSuffix("\nSet8\n"sv, &cont)) {
    return (string)cont;
  }
  if (TryStripSuffix("\nEnjoy!\n"sv, &cont)) {
    return (string)cont;
  }
  if (TryStripSuffix("\nenjoy!\n"sv, &cont)) {
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
  static LazyRE2 chordsof = {
    "\nlyrics and chords of: (.+) by (.+)\n"
  };
  RE2::GlobalReplace(&s, *chordsof, "\nTitle: \\1\n"
		     "Artist: \\2\n");

  static LazyRE2 recordedby = {
    "\n[ \t]*As recorded by (.+)\n"
  };
  RE2::GlobalReplace(&s, *recordedby, "\nArtist: \\1\n");
  
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

string Frontslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '\\' ? '/' : c);

  if (ret.find("d:/") == 0) {
    ret[0] = '/';
    ret[1] = 'd';
  } else if (ret.find("c:/") == 0) {
    ret[0] = '/';
    ret[1] = 'c';
  }

  return ret;
}

string Backslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '/' ? '\\' : c);
  return ret;
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
  return r;
}

int main(int argc, char **argv) {
 
  vector<string> all_filenames;
  for (const char *d : DIRS) {
    AddAllFilesRec(d, &all_filenames);
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
		  /*
		  bool accept =
		    row.second.find("-PLEASE NOTE-") != string::npos ||
		    row.second.find("scholarship, or research") != string::npos ||
		    row.second.find("Have fun, DAIRYBEAT on") != string::npos ||
		    row.second.find("and has a specific punishment") != string::npos ||
		    // XXX
		    // HasNonAscii(row.second) ||
		    row.second.find("Song/CD") != string::npos ||
		    row.second.find("in an unauthorized application,") != string::npos;
		  */
		  string title, artist;
		  bool accept = !p.GetMetadata(row.first, row.second, &title, &artist);
		  
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
		[&p, &m, &has_extraction, &examples](const pair<string, string> &row) {
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
		[&p, &m, &has_extraction, &examples](const pair<string, string> &row) {
		  string title, artist;
		  string contents = row.second;
		  vector<pair<string, string>> orig_hdrs = p.ExtractHeaders(&contents);
		  if (!orig_hdrs.empty()) {
		    vector<pair<string, string>> hdrs = orig_hdrs;
		    p.FilterHeaders(&hdrs);
		    p.NormalizeHeaders(&hdrs);
		    p.SortHeaders(&hdrs);

		    if (orig_hdrs != hdrs) {
		      string ex = row.first + (string)"\n";
		      for (const auto &p : hdrs) {
			ex += StringPrintf("  [%s] = [%s]\n", p.first.c_str(), p.second.c_str());
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
		  vector<pair<string, string>> orig_hdrs = p.ExtractHeaders(&contents);
		  if (!orig_hdrs.empty()) {
		    vector<pair<string, string>> hdrs = orig_hdrs;
		    p.FilterHeaders(&hdrs);
		    p.NormalizeHeaders(&hdrs);
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
				       key.c_str(), value.c_str(), file.c_str()));
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

  // return 0;
  
  {
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
		  c = p.FixHeaders(c);
		  c = StripFooter(c);
		  /*
		    if (!c.empty() && c[0] == ' ') {
		    c = c.substr(1);
		    }
		  */
		  
		  if (c != row.second) {
		    string filename = Backslash(row.first);
		    if (DRY_RUN) {
		      // XXX show full lines!
		      string s1 = row.second;
		      string s2 = c;
		      int same_begin = 0, same_end = 0;
		      while (!s1.empty() && !s2.empty() &&
			     s1.back() == s2.back()) {
			s1.pop_back();
			s2.pop_back();
			same_end++;
		      }

		      int start = 0;
		      while (start < (int)s1.size() &&
			     start < (int)s2.size() &&
			     s1[start] == s2[start]) {
			start++;
			same_begin++;
		      }
		      s1 = s1.substr(start, string::npos);
		      s2 = s2.substr(start, string::npos);
		      
		      MutexLock ml(&m);
		      previews.push_back(
			  StringPrintf("** %s\n"
				       "(+%d before)\n"
				       "%s\n"
				       "  ->\n"
				       "%s\n"
				       "(+%d after)\n",
				       filename.c_str(),
				       same_begin,
				       s1.c_str(), s2.c_str(), same_end));
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
  
  printf("Done.\n");
  fflush(stdout);

  // ComputeDistances(files);
  
  return 0;
}
