/* -------=[ loop.cc ]=------------------------------------------------ -

          Loop C-like compiler for Ludus objects

          Producing lasm output code

 - ---------------------------------=[ LUDUS ]=------------------------ */

#include <string>
#include "loop.h"

string parsestatementblock (string block, string reg_return_value_in);

main (int argc, char ** argv) {
     FILE * source = stdin,
          * dest   = stdout;

     if (argc >= 2)
          if (!(source = fopen(argv[1],"r"))) error((string)"Cannot open input file " + (string)argv[1] + (string) "!");
     if (argc >= 3)
          if (!(dest = fopen(argv[2],"wb"))) error((string)"Cannot open output file " + (string)argv[2] + (string) "!");
     
}
