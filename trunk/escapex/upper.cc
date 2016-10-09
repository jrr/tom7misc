
#include "upper.h"
#include "util.h"
#include "directories.h"
#include "../cc-lib/md5.h"
#include "chars.h"
#include "dirindex.h"
#include "ptrlist.h"
#include "hashtable.h"

#define HASHSIZE 1021

namespace {

struct OldEntry {
  /* suitable for open() */
  string fname;

  bool deleteme;

  string key() {
    return fname;
  }

  static unsigned int hash(string k) {
    return util::hash(k);
  }

  void destroy() { delete this; }

  OldEntry(string f) : fname(f), deleteme(true) {}
};

struct ContentEntry {
  string md5;
  string content;

  string key() {
    return md5;
  }

  /* endianness doesn't matter; we just have
     to be consistent */
  /* poorer than it could be because
     this string is in ASCII */
  static unsigned int hash(string k) {
    return *((unsigned int*) k.c_str());
  }

  void destroy() { delete this; }

  ContentEntry(string con) : content(con) {
    md5 = MD5::Ascii(MD5::Hash(con));
  }

};

using oldtable = hashtable<OldEntry, string>;
using contable = hashtable<ContentEntry, string>;

struct Upper_ : public Upper {
  static Upper_ *Create(HTTP *, TextScroll *, Drawable *, string);

  ~Upper_() override;

  bool setfile(string f, string md, RateStatus votes,
               int, int, int o) override;
  bool commit() override;

  void savedir(string d, string index) override;

  void redraw() {
    if (below) {
      below->draw();
      SDL_Flip(screen);
    }
  }

  void say(string s, bool nodraw = false) {
    if (tx) {
      tx->say(s);
      if (!nodraw) redraw();
    }
  }

  void sayover(string s, bool nodraw = false) {
    if (tx) {
      tx->unsay();
      tx->say(s);
      if (!nodraw) redraw();
    }
  }

  /* for reporting progress */
  TextScroll *tx;
  Drawable *below;

  oldtable *olds;
  contable *contents;

  HTTP *hh;

  /* the directory, like "official" */
  string dirname;

  /* list of new files: filename, md5 */
  stringlist *newlistf;
  stringlist *newlistm;

  /* list of saved dirs: dir, index.
     rep invt: these are the same length */
  stringlist *dirlistd;
  PtrList<DirIndex> *dirlisti;

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

  olds = oldtable::create(HASHSIZE);
  contents = contable::create(HASHSIZE);

  newlistf = 0;
  newlistm = 0;

  dirlistd = 0;
  dirlisti = 0;

  /* PERF: could download less if we
     included more directories in this
     search. But we don't want to delete
     from them! */
  /* loop over every file in dirname */

  insertdir(dirname);
}

void Upper_::savedir(string d, string i) {
  /* XXX: could fail if d is a file. In this
     case we're sort of in trouble, since we
     can't move d without invalidating our own
     contable. */
  if (d != "") util::makedir(dirname + (string)DIRSEP + d);
  stringlist::push(dirlistd, d);

  /* XXX error checking? */
  DirIndex *di = DirIndex::Create();
  di->title = i;
  PtrList<DirIndex>::push(dirlisti, di);
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

    if (util::isdir(f)) {
      insertdir(f);
    } else {
      olds->insert(new OldEntry(f));

      /* XXX use readfilesize,
         where it won't read the file
         unless it is small. (Someone
         might stick big files in
         managed dirs...)
      */
      string inside = readfile(f);
      contents->insert(new ContentEntry(inside));
    }
  }

  closedir(d);
}

Upper_::~Upper_() {
  stringlist::diminish(newlistf);
  stringlist::diminish(newlistm);

  contents->destroy();
  olds->destroy();
}

bool Upper_::setfile(string f, string md, RateStatus votes,
                     int date, int speedrecord, int owner) {
  say((string)"setfile(" + f + (string)", "
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

  ContentEntry *already = contents->lookup(md);

  /* if it's not already in the content hashtable,
     get it from the internet. */
  if (!already) {
    string mm;

    string first = md.substr(0, 2);
    string last  = md.substr(2, md.length() - 2);

    httpresult hr =
      hh->get((string)"/" + dirname +
              (string)"/" + first +
              (string)"/" + last, mm);

    switch (hr) {
    case HT_OK: {
      ContentEntry *nce = new ContentEntry(mm);
      sayover((string)"(setfile) downloaded : " + nce->md5, true);

      if (nce->md5 != md) {
        say(RED "what I got differs from expected");
        say("written to last_got for debug");
        /* debug */
        writefile("last_got", mm);
        return false;
      }
      contents->insert(nce);
      break;
    }

    default:
      say((string)RED "unable to download");
      return false;
    }
  } else {
    sayover((string)"(setfile) already exists : "
            BLUE + f + (string)POP" "
            GREY + md + (string)POP, true);
  }

  /* if it's in the olds, mark it so
     that it won't be deleted */

  OldEntry *existing = olds->lookup(dirname + DIRSEP + f);
  if (existing) existing->deleteme = false;

  /* put it in newlist */

  stringlist::push(newlistf, f);
  stringlist::push(newlistm, md);

  /* add its rating to the index.
     first, figure out what directory it lives in. */
  {
    string dd = util::pathof(f);
    string ff = util::fileof(f);
    /* we designate the current dir with the empty string, instead */
    if (dd == ".") dd = "";

    stringlist *dt = dirlistd;
    PtrList<DirIndex> *it = dirlisti;

    while (dt && it) {

      // printf("compare [%s] [%s]\n", dt->head.c_str(), dd.c_str());
      if (dt->head == dd) {
        it->head->addentry(ff, votes, date, speedrecord, owner);
        return true;
      }

      dt = dt->next;
      it = it->next;
    }
    /* XXX should fail if directory not found? */
  }

  return true;
}

static void deleteif(OldEntry *oe, int dummy_param) {
  if (oe->deleteme) {
    /* we can delete index files with proper magic. these are
       always overwritten with every update */
    if (DirIndex::isindex(oe->fname)) {
      /* but if deletion fails (in use?), try moving */
      if (!util::remove(oe->fname))
        util::toattic(oe->fname);
    } else {
      util::toattic(oe->fname);
    }
  }
}

bool Upper_::commit() {
  say(YELLOW " ======= " WHITE " commit phase " POP " ======= " POP);

  /* overwrite anything in newlist. */

  /* PERF optimization: don't do anything if
     content in oldf is the same as what
     we're going to write over it. this
     should be the common case... (to do this,
     store md5 in olds) */

  string nlf;
  string nlm;

  for (;;) {
    nlf = stringpop(newlistf);
    nlm = stringpop(newlistm);

    if (nlf == "") {
      if (nlm == "") break; /* done */
      say(RED "inconsistent lengths of nlf/nlm??" POP);
      return false;
    }

    /* everything is rooted within dirname */
    nlf = dirname + DIRSEP + nlf;

    /* Try removing before opening; Adam seems to think this
       improves our chances of success. */
    util::remove(nlf);
    FILE *a = util::fopenp(nlf, "wb");
    if (!a) {
      say((string)RED "couldn't write " + nlf + POP);
      /* XXX should continue writing, just not delete? */
      return false;
    }

    ContentEntry *ce = contents->lookup(nlm);

    if (!ce) {
      say((string)RED "bug: md5 " BLUE "[" + nlm +
          (string)"]" POP " isn't in table now??");

      fclose(a);
      return false;
    } else {
      if (1 != fwrite(ce->content.c_str(), ce->content.length(), 1, a)) {
        say((string)RED "couldn't write to " BLUE + nlf +
            (string)POP " (disk full?)");

        fclose(a);
        return false;
      } else {
        /*
          say((string)GREEN "wrote " BLUE + nlf +
          (string)POP " <- " GREY + nlm + (string)POP" ok"); */
      }
    }

    fclose(a);

  }

  /* delete anything with delme=true in olds */
  hashtable_app(olds, deleteif, 0);

  /* create indices */
  /* We have the invt that length(dirlistd) =
     length(dirlisti) */
  while (dirlistd) {
    string d = stringpop(dirlistd);
    std::unique_ptr<DirIndex> i{PtrList<DirIndex>::pop(dirlisti)};

    string f =
      (d == "") ?
      (dirname + (string)DIRSEP WEBINDEXNAME) :
      dirname + (string)DIRSEP + d + (string)DIRSEP WEBINDEXNAME;

    /* XXX check failure? */
    i->writefile(f);
  }

  /* FIXME prune empty dirs */

  return true;
}

}  // namespace

Upper *Upper::Create(HTTP *h, TextScroll *t,
                      Drawable *d, string f) {
  return Upper_::Create(h, t, d, f);
}
