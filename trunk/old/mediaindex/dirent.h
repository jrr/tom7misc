
/*
    Derived from source copyright Kevlin Henney, 1997, 2003. 
    All rights reserved.

    THIS FILE ONLY:

    Permission to use, copy, modify, and distribute this software and its
    documentation for any purpose is hereby granted without fee, provided
    that this copyright and permissions notice appear in all copies and
    derivatives.
    
    This software is supplied "as is" without express or implied warranty.

    But that said, if there are any problems please get in touch.

*/

#ifndef __DIRENT_H
#define __DIRENT_H

#ifndef WIN32
/* posix */
#  include <sys/types.h>
#  include <dirent.h>
#  include <unistd.h>
#else


#ifdef __cplusplus
extern "C"
{
#endif

typedef struct DIR DIR;

struct dirent {
    char *d_name;
};

DIR           *opendir(const char *);
int           closedir(DIR *);
struct dirent *readdir(DIR *);
void          rewinddir(DIR *);

#ifdef __cplusplus
}
#endif

#endif /* win32 */

#endif
