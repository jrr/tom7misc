
#include "escapex.h"
#include "startup.h"
#include "directories.h"
#include "escape-util.h"

#ifndef WIN32
/* for symlink */
# include <unistd.h>
#endif

#define DOTESCAPE ".escape"

#ifdef MULTIUSER
# ifndef STARTUP_LEVELS
#  error "If multiuser, STARTUP_LEVELS must be defined (path to initial level structure)"
# endif
#endif

/* First order of business is to set the current working directory.
   On a single-user install, we change to the directory where the
   escape executable is.

   On a multi-user install, we switch to the user's home directory
   based on that system's conventions.
*/
#ifndef MULTIUSER
 string StartUp::self;
#endif

/* copy (or symlink) all levels (recursively) into the current directory. */
/* (no trailing / on path) */
int StartUp::install_levels(string path) {
  DIR *d = opendir(path.c_str());
  if (!d) return 0;
  dirent *de;

  while ( (de = readdir(d)) ) {
    string dn = de->d_name;
    string full = path + (string)DIRSEP + dn;

    if (dn == "." || dn == "..") {
      /* skip */
    } else if (EscapeUtil::isdir(full)) {
      /* in current directory... */
      // printf("makedir %s\n", dn.c_str());
      if (!EscapeUtil::makedir(dn)) return 0;
      /* and go there... */
      // printf("chdir %s\n", dn.c_str());
      if (!EscapeUtil::changedir(dn)) return 0;

      /* copy all the files recursively */
      if (!install_levels(full)) return 0;

      // printf("chdir ..\n");
      if (!EscapeUtil::changedir("..")) return 0;
    } else {
      /* regular file; copy or symlink as appropriate */
      // printf("copy %s -> %s\n", full.c_str(), dn.c_str());
#     if WIN32
      if (!EscapeUtil::copy(full, dn)) return 0;
#     else
      if (symlink(full.c_str(), dn.c_str())) return 0;
#     endif
    }
  }
  return 1;
}

int StartUp::setdir(int argc, char **argv) {
# ifdef MULTIUSER
#  ifdef LINUX

  if (!EscapeUtil::changedir(getenv("HOME"))) {
    printf("Can't change to home directory!\n");
    return 0;
  }

  if (!EscapeUtil::existsdir(DOTESCAPE)) {
    /* first startup. */
    if (EscapeUtil::makedir(DOTESCAPE)) {
      printf("Created " DOTESCAPE " ...\n");
      if (EscapeUtil::changedir(DOTESCAPE) && install_levels(STARTUP_LEVELS)) {
        printf("Installed levels...\n");
      } else {
        printf("Warning: wasn't able to create local cache of levels\n");
      }
    } else {
      printf("Can't create " DOTESCAPE " in home directory!\n");
      return 0;
    }

  }

  /* now change into the directory we created */
  if (!(EscapeUtil::changedir(getenv("HOME")) &&
        EscapeUtil::changedir(DOTESCAPE))) {
    printf("Can't change into " DOTESCAPE " directory!\n");
    return 0;
  }

  return 1;

#  else
#   error "MUTLIUSER setdir not implemented on this PLATFORM"
#  endif
# else /* single-user */

  if (argc > 0) {
    string wd = EscapeUtil::pathof(argv[0]);
    EscapeUtil::changedir(wd);

#   if WIN32
    /* on win32, the ".exe" may or may not
       be present. Also, the file may have
       been invoked in any CaSe. */

    self = EscapeUtil::lcase(EscapeUtil::fileof(argv[0]));

    self = EscapeUtil::ensureext(self, ".exe");

#   else
    self = EscapeUtil::fileof(argv[0]);
#   endif

  }

  return 1;
# endif
}

