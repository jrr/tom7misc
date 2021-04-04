
#ifndef _ESCAPE_CLEANUP_H
#define _ESCAPE_CLEANUP_H

/* looks through the current directory
   for any files named *.deleteme,
   and deletes them. also ensures that
   some expected game dirs actually exist,
   such as 'mylevels' */
struct Cleanup {
  static void Clean();
};

#endif
