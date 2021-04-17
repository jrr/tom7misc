
#include "cleanup.h"

#include <sys/stat.h>

#include "escapex.h"
#include "escape-util.h"
#include "directories.h"
#include "edit.h"
#include "dircache.h"

#include "../cc-lib/util.h"

void Cleanup::Clean() {
  DIR *d = opendir(".");
  if (!d) return;
  dirent *dire;

  while ( (dire = readdir(d)) ) {

    string dn = dire->d_name;
    string ldn = "." + (string)DIRSEP + dn;

    if (EscapeUtil::isdir(ldn)) {
      /* XXX could be recursive?? */

    } else {

      if (EscapeUtil::endswith(dn, ".deleteme")) {
        /* printf(" remove '%s'\n", ldn.c_str()); */
        EscapeUtil::remove(ldn);
      }
    }
  }

  /* harmless if it fails, unless there is a *file*
     called mylevels, in which case we are screwed... */
  EscapeUtil::makedir(EDIT_DIR);

  /* make attic dir. Make sure it is ignored. */
  EscapeUtil::makedir(ATTIC_DIR);
  Util::WriteFile((string)ATTIC_DIR + DIRSEP + IGNOREFILE, "");

  closedir(d);
}
