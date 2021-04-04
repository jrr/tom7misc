#include "upper.h"

#include <unordered_map>
#include <vector>
#include <string>

#include "escape-util.h"
#include "directories.h"
#include "../cc-lib/crypt/md5.h"
#include "chars.h"
#include "dirindex.h"

namespace {

struct OldEntry {
  /* suitable for open() */
  string fname;

  bool deleteme = false;

  OldEntry(string f) : fname(f), deleteme(true) {}
};

struct ContentEntry {
  string md5;
  string content;

  explicit ContentEntry(string con) : content(std::move(con)) {
    md5 = MD5::Ascii(MD5::Hash(content));
  }
};

struct Upper_ : public Upper {
  static Upper_ *Create(HTTP *, TextScroll *, Drawable *, string);

  ~Upper_() override;

  bool SetFile(const string &f, const string &md, RateStatus votes,
               int, int, int o) override;
  bool Commit() override;

  void SaveDir(const string &d, const string &index) override;

  void Redraw() {
    if (below) {
      below->Draw();
      SDL_Flip(screen);
    }
  }

  void say(string s, bool nodraw = false) {
    if (tx) {
      tx->Say(s);
      if (!nodraw) Redraw();
    }
  }

  void sayover(string s, bool nodraw = false) {
    if (tx) {
      tx->Unsay();
      tx->Say(s);
      if (!nodraw) Redraw();
    }
  }

  /* for reporting progress */
  TextScroll *tx;
  Drawable *below;

  std::unordered_map<string, OldEntry> olds;
  std::unordered_map<string, ContentEntry> contents;

  HTTP *hh;

  /* the directory, like "official" */
  string dirname;

  /* list of new files: filename, md5 */
  std::vector<pair<string, string>> newlist;

  /* list of saved dirs: dir, index. */
  std::vector<pair<string, std::unique_ptr<DirIndex>>> dirlist;

  void init();
  void insertdir(string d);
};

Upper_ *Upper_::Create(HTTP *h, TextScroll *t,
                        Drawable *d, string f) {
  Upper_ *ur = new Upper_();
  ur->hh = h;
  ur->tx = t;
  ur->below = d;
  ur->dirname = f;

  ur->init();

  return ur;
}

void Upper_::init() {
  /* initialize 'olds' and 'contents' */

  /* PERF: could download less if we
     included more directories in this
     search. But we don't want to delete
     from them! */
  /* loop over every file in dirname */

  insertdir(dirname);
}

void Upper_::SaveDir(const string &d, const string &i) {
  /* XXX: could fail if d is a file. In this
     case we're sort of in trouble, since we
     can't move d without invalidating our own
     contable. */
  if (d != "") EscapeUtil::makedir(dirname + (string)DIRSEP + d);
  /* XXX error checking? */
  DirIndex *di = DirIndex::Create();
  di->title = i;
  dirlist.emplace_back(d, di);
}

void Upper_::insertdir(string src) {
  DIR *d = opendir(src.c_str());

  say((string)YELLOW"insertdir " + src + POP);

  struct dirent *de;
  while ( (de = readdir(d)) ) {

    string basef = (string)de->d_name;
    string f = src + (string)DIRSEP + basef;

    /* say(f); */

    /* ignore some stuff */
    if (basef == "" /* ?? */ ||
        basef == "." ||
        basef == ".." ||
        basef == ".svn" ||
        basef == "CVS") continue;

    if (EscapeUtil::isdir(f)) {
      insertdir(f);
    } else {
      olds.insert({f, OldEntry(f)});

      /* XXX use readfilesize,
         where it won't read the file
         unless it is small. (Someone
         might stick big files in
         managed dirs...)
      */
      ContentEntry entry{readfile(f)};
      contents.insert({entry.md5, std::move(entry)});
    }
  }

  closedir(d);
}

Upper_::~Upper_() {
}

bool Upper_::SetFile(const string &ff, const string &md, RateStatus votes,
                     int date, int speedrecord, int owner) {
  string f = ff;
  say((string)"SetFile(" + f + (string)", "
      GREY + md + (string)POP ")", true);

  /* check that f is legal */
  if (f == "") return false;
  if (f[0] == DIRSEPC) return false;
  if (f.find("..") != string::npos) return false;

# ifdef WIN32
  /* XXX should just replace / with dirsep, unconditionally */
  /* on windows, replace / with \ */
  for (unsigned int j = 0; j < f.length(); j++) {
    if (f[j] == '/') f[j] = '\\';
  }
# endif

  /* if it's not already in the content hashtable,
     get it from the internet. */
  if (contents.find(md) == contents.end()) {
    string mm;

    string first = md.substr(0, 2);
    string last  = md.substr(2, md.length() - 2);

    HTTPResult hr =
      hh->get((string)"/" + dirname +
              (string)"/" + first +
              (string)"/" + last, mm);

    switch (hr) {
    case HTTPResult::OK: {
      ContentEntry nce{mm};
      sayover((string)"(SetFile) downloaded : " + nce.md5, true);

      if (nce.md5 != md) {
        say(RED "what I got differs from expected");
        say("written to last_got for debug");
        /* debug */
        writefile("last_got", mm);
        return false;
      }
      contents.insert({nce.md5, std::move(nce)});
      break;
    }

    default:
      say((string)RED "unable to download");
      return false;
    }
  } else {
    sayover((string)"(SetFile) already exists : "
            BLUE + f + (string)POP" "
            GREY + md + (string)POP, true);
  }

  /* if it's in the olds, mark it so
     that it won't be deleted */
  {
    auto it = olds.find(dirname + DIRSEP + f);
    if (it != olds.end()) it->second.deleteme = false;
  }

  /* put it in newlist */
  newlist.emplace_back(f, md);

  /* add its rating to the index.
     first, figure out what directory it lives in. */
  {
    string dd = EscapeUtil::pathof(f);
    string ff = EscapeUtil::fileof(f);
    /* we designate the current dir with the empty string, instead */
    if (dd == ".") dd = "";

    for (const auto &[ddir, dindex] : dirlist) {
      // printf("compare [%s] [%s]\n", dt->head.c_str(), dd.c_str());
      if (ddir == dd) {
        dindex->AddEntry(ff, votes, date, speedrecord, owner);
        return true;
      }
    }
    /* XXX should fail if directory not found? */
  }

  return true;
}

static void DeleteIf(const OldEntry &oe) {
  if (oe.deleteme) {
    /* we can delete index files with proper magic. these are
       always overwritten with every update */
    if (DirIndex::IsIndex(oe.fname)) {
      /* but if deletion fails (in use?), try moving */
      if (!EscapeUtil::remove(oe.fname))
        EscapeUtil::toattic(oe.fname);
    } else {
      EscapeUtil::toattic(oe.fname);
    }
  }
}

bool Upper_::Commit() {
  say(YELLOW " ======= " WHITE " commit phase " POP " ======= " POP);

  /* overwrite anything in newlist. */

  /* PERF optimization: don't do anything if
     content in oldf is the same as what
     we're going to write over it. this
     should be the common case... (to do this,
     store md5 in olds) */

  for (const auto &[newlist_file, newlist_md5] : newlist) {
    /* everything is rooted within dirname */
    const string nlf = dirname + DIRSEP + newlist_file;

    /* Try removing before opening; Adam seems to think this
       improves our chances of success. */
    EscapeUtil::remove(nlf);
    FILE *a = EscapeUtil::fopenp(nlf, "wb");
    if (!a) {
      say((string)RED "couldn't write " + nlf + POP);
      /* XXX should continue writing, just not delete? */
      return false;
    }

    {
      auto it = contents.find(newlist_md5);
      if (it == contents.end()) {
	say((string)RED "bug: md5 " BLUE "[" + newlist_md5 +
	    (string)"]" POP " isn't in table now??");

	fclose(a);
	return false;
      } else {
	const ContentEntry &ce = it->second;
	if (1 != fwrite(ce.content.data(), ce.content.length(), 1, a)) {
	  say((string)RED "couldn't write to " BLUE + nlf +
	      (string)POP " (disk full?)");

	  fclose(a);
	  return false;
	} else {
	  /*
	    say((string)GREEN "wrote " BLUE + nlf +
	    (string)POP " <- " GREY + newlist_md5 + (string)POP" ok"); */
	}
      }
    }

    fclose(a);
  }

  /* delete anything with delme=true in olds */
  for (const auto &p : olds)
    DeleteIf(p.second);

  /* create indices */
  for (const auto &[dir, index] : dirlist) {
    const string f =
      (dir == "") ?
      (dirname + (string)DIRSEP WEBINDEXNAME) :
      dirname + (string)DIRSEP + dir + (string)DIRSEP WEBINDEXNAME;

    /* XXX check failure? */
    index->WriteFile(f);
  }

  dirlist.clear();

  /* FIXME prune empty dirs */

  return true;
}

}  // namespace

Upper *Upper::Create(HTTP *h, TextScroll *t,
		     Drawable *d, const string &f) {
  return Upper_::Create(h, t, d, f);
}
