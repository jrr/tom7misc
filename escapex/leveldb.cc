
/* n.b.: This code is not hooked up yet. You probably want to look at
   dircache.cc if you want to see what's actually going on in 3.0 series.
*/

#include "escapex.h"
#include "leveldb.h"
#include "directories.h"
#include "../cc-lib/md5.h"
#include "SDL.h"
#include "player.h"
#include "message.h"
#include "progress.h" // XXX

#include <vector>
#include <map>
#include <string>

/* levels loaded from disk and waiting to be added to the database */
struct LevelWait {
  Level *l;
  /* Just a regular filename, but we should support collection files
     eventually. */
  string filename;
  string md5;
  LevelWait(Level *le, string fn, string m) : l(le), filename(fn), md5(m) {}
  LevelWait() : l(0) {}
};

/* Must be initialized before adding any sources. */
static Player *theplayer = nullptr;

/* these are actually treated as stacks, but there's
   nothing wrong with that... */
/* files waiting to be added into the level database */
static stringlist *filequeue = nullptr;
static int filequeue_size = 0;

/* loaded levels waiting to be added into the database
   (need to verify solutions), etc. */
static PtrList<LevelWait> *levelqueue = nullptr;
static int levelqueue_size = 0;

/* All levels that we've loaded, as a map from MD5 to the level
   database entry. Each one is allocated once and can be referred to
   efficiently in that canonical location. */
static map<string, res_level*> all_levels;

void LevelDB::setplayer(Player *p) {
  theplayer = p;
}

/* XXX should enqueue the directory to be processed
   here instead of doing it immediately, since directories could
   be arbitrarily large. */
/* XXX should be recursive too, I guess. */
/* XXX if .escignore is present, stop */
void LevelDB::addsourcedir(string s) {
  if (!theplayer) abort();
  int count = 0;
  DIR *d = opendir(s.c_str());
  dirent *de;
  while ( (de = readdir(d)) ) {
    if (strcmp(".", de->d_name) &&
        strcmp("..", de->d_name)) {
      count++;
      addsourcefile(util::dirplus(s, de->d_name));
    }
  }

  fprintf(stderr, "added %d files from %s\n", count, s.c_str());
}

void LevelDB::addsourcefile(string s) {
  if (!theplayer) abort();
  filequeue = new stringlist(s, filequeue);
  filequeue_size++;
}

bool LevelDB::uptodate(float *pct_disk, float *pct_verify) {
  // PERF: is map<>.size constant-time?
  int total = levelqueue_size + filequeue_size + all_levels.size();

  if (pct_disk) {
    if (total) {
      *pct_disk = 1.0 - float(filequeue_size) / total;
        } else {
      *pct_disk = 1.0;
    }
  }

  if (pct_verify) {
    if (total) {
      *pct_verify = 1.0 - float(levelqueue_size + filequeue_size) / total;
    } else {
      *pct_verify = 1.0;
    }
  }

  fprintf(stderr, "lq %d fq %d\n", levelqueue_size, filequeue_size);
  return levelqueue_size == 0 && filequeue_size == 0;
}

void LevelDB::donate(int max_files, int max_verifies, int max_ticks) {
  // message::bug(0, "DONATE.\n");

  int files_left = max_files;
  int verifies_left = max_verifies;
  unsigned int gameover = SDL_GetTicks() + max_ticks;

  do {
    if (levelqueue &&
        (!max_verifies ||
         verifies_left > 0)) {

      fprintf(stderr, "Do verify.\n");
      verifies_left--;

      std::unique_ptr<LevelWait> lw {PtrList<LevelWait>::pop(levelqueue)};
      levelqueue_size--;

      if (lw.get() == nullptr) abort();

      res_level *entry = util::findorinsertnew(all_levels, lw->md5);

      if (!entry || lw->md5.empty()) abort();

      entry->md5 = lw->md5;
      // XXX should avoid doing this if it's already there. Levels
      // could be inserted twice, right?
      entry->sources.push_back(lw->filename);

      // XXX entry.date (from web thingy)
      // XXX entry.speedrecord (from player)
      // XXX entry.___votes (from web thingy)
      // XXX owned_by_me (from web thingy)

      // If this is the second time we're loading it,
      // get rid of the duplicate. Prefer the old one
      // in case someone already has an alias to it.
      if (entry->lev == 0) entry->lev = lw->l;
      else delete lw->l;

      fprintf(stderr, "Inserted level %p from %s\n",
              entry, lw->filename.c_str());

      /* Verify the solution now so that we can get quicker access to
         it later. Should we do this for all solutions? */
      if (const Solution *s = theplayer->GetSol(entry->md5)) {
        if (!s->verified && Level::Verify(entry->lev, *s)) {
          theplayer->SetDefaultVerified(entry->md5);
          fprintf(stderr, "  valid solution for level %s\n",
                  MD5::Ascii(entry->md5).c_str());
        }
      }

    } else if (filequeue &&
               (!max_files ||
                files_left > 0)) {

      fprintf(stderr, "Do file.\n");

      files_left--;
      string s = stringpop(filequeue);
      filequeue_size--;

      /* XXX else could be a multilevel file, once we
         support those. */
      if (util::hasmagic(s, LEVELMAGIC)) {
        string c = readfile(s);
	std::unique_ptr<Level> l = Level::FromString(c, true);
        if (l.get() != nullptr) {
          const string m = MD5::Hash(c);
          /* put on the level queue now */
          levelqueue = new PtrList<LevelWait>(
	      new LevelWait(l.get(), s, m), levelqueue);
          levelqueue_size++;

          fprintf(stderr, "Enqueued level from %s\n", s.c_str());
              } else {
          fprintf(stderr, "%s is not a level\n", s.c_str());
        }
      } else {
        fprintf(stderr, "%s does not have the magic\n", s.c_str());
      }

    } else {
      /* No more file/verify budget, or no more work to do. */
      break;
    }
  } while (!max_ticks || SDL_GetTicks() < gameover);
  /* No more file/verify/time budget, or no more work to do. */
}


/* Query processing.
   Maybe this belongs in leveldb-query.cc? */
#if 0
static lval getfield(const lentry *l, const lfield f) {
  switch (f) {
  case LF_TITLE: return lval(l->lev->title);
  case LF_AUTHOR: return lval(l->lev->author);
  case LF_WIDTH: return lval(l->lev->w);
  case LF_HEIGHT: return lval(l->lev->h);
  case LF_MD5: return lval(l->md5);
  case LF_DATE: return lval(l->date);
  case LF_SOLVED: return lval(l->solved);
  default: return lval::exn("unimplemented field");
  }
}

lval lexp::eval(const lentry *l) {
  switch (tag) {
  case LE_VALUE: return v;
  case LE_FIELD: return getfield(l, f);

  default: return lval::exn("unimplemented");
  }
}

#endif
