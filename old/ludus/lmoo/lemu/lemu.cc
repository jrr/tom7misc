
/* --------=[ lemu.cc ]=------------------------------------------------ -

          Ludus Object Emulator

          An environment for rudimentary debugging and experimentation

 - ---------------------------------=[ LUDUS ]=------------------------ */

/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */


#include "lob.h"
#include "func.h"
#include <iostream.h>
#include <string>

void global_init();

main () {
     global_init();

     lob obbie("rot13");
     obbie.init();
     cout << obbie.invokemethod("BORN") << endl;
     while (obbie.running) obbie.run(3);
     cout << obbie.invokemethod("TOUCH") << endl;
     while (obbie.running) obbie.run(3);
     cout << "Done.\n";
}

void global_init() {
     functions_build ();

}
