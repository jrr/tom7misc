/* --------=[ lob.cc ]=------------------------------------------------- -

          Ludus Object class

          Class for running ludus objects

   ---------------------------------=[ LUDUS ]=------------------------ */

#include "lob.h"
#include "opcodes.h"
#include <iostream.h>
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string>

#define FUNC(c,d) {(string)#c,d,NULL,NULL},

lob_func * functions = NULL;

lob_func insertus[] = {
     FUNC(SPAWNSOUND,  spawnsound)
     FUNC(SPARKLES,    sparkles)
     FUNC(SAYMSG,      saymsg)
     {(string)"", NULL, NULL, NULL},   /* MUST END LIST */
};

void functions_build () {
     /* must be called first! */
     int x=0;
     while(insertus[x].func) {
          install_function(
               new lob_func((lob_func){insertus[x].name,
                                       insertus[x].func,
                                      })
                           );
     x++;
     }
}

void install_function(lob_func * s) {
     lob_func ** f = &functions;
     while (*f) {
          if( s->name < (*f)->name ) f = &((*f)->left );
                              else   f = &((*f)->right);
     }
     *f = s;
}

func_t lob :: callfunction (string name) {
    
     lob_func * f = functions;
     while (f) {
//     cout << "[callfunction] I am " << f->name << ", look for: " << name << endl;
          if (name == f->name) {
               return f->func(this);
          } else if (name < f->name) f = f->left ;
            else                     f = f->right;
     }
     return FUNC_NOTFOUND;
}

/***** HERE GO THE FUNCTIONS *****/

LOBFUNC(saymsg) {
    cout <<  "   " << obj->objname << ".saymsg: "
         <<  ((char *) (obj->program.c_str()+obj->outarg[0]))
         << endl;
    return FUNC_OK;
}

LOBFUNC(spawnsound) {
    cout <<  "   " << obj->objname << ".spawnsound: "
         <<  ((char *) (obj->program.c_str()+obj->outarg[0]))
         << endl;
    return FUNC_OK;
}

LOBFUNC(sparkles) {
    cout <<  "   (" << obj->objname << ".sparkles) "
         << endl;
    return FUNC_OK;
}
