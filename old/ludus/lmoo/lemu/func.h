
/* --------=[ func.h ]=------------------------------------------------- -

          Ludus object functions                      header file

          Functions callable by ludus objects

   ---------------------------------=[ LUDUS ]=------------------------ */


#ifndef __TM7_LUDUS_FUNC_H
#define __TM7_LUDUS_FUNC_H

enum func_t { FUNC_NIL, FUNC_OK, FUNC_CRIT, FUNC_NOTFOUND, };

#define LOBFUNC(c) func_t c(lob * obj)

#include "lob.h"

class lob;

struct lob_func {
     string name;
     func_t (*func)(lob * obj);
     lob_func * left,
              * right;
};

void install_function(lob_func*),
     functions_build ();

LOBFUNC(saymsg);
LOBFUNC(spawnsound);
LOBFUNC(sparkles);

#endif
