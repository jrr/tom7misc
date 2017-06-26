
/* --------=[ lob.h ]=-------------------------------------------------- -

          Ludus Object class                      header file

          Class for running ludus objects

   ---------------------------------=[ LUDUS ]=------------------------ */

#ifndef __TM7_LUDUS_LOB_H
#define __TM7_LUDUS_LOB_H

#include <string>
#include "func.h"

#define uint unsigned int

struct method {
     string name;
     unsigned short loc;
     method * next;
};

enum invoke_t { INVOKE_NUL, INVOKE_OK, INVOKE_LOCKED, INVOKE_NOMETHOD, };

class lob {

public: /* YEahhh... real man style! */

     uint IP;
     unsigned short regs[5], /* A-D, R */
                    inarg[10], /* INARG1-INARG10 */
                    outarg[10]; /* OUTARG1-OUTARG10 */

   func_t callfunction(string name);

     void register_method(const char * name, int address),
          setlvalue(int start, unsigned short value),
          error(string s);

void printregs ();

void printbin ( const char * s, int len ),
     printbins( string s);

     unsigned short getword (int start),
                    getbyte (int start),
                    getvalue(int start);
     
     unsigned short * register_addr (unsigned char x);

     method * methods;
     string program;

     unsigned short (lob::*valfuncs[2])(int);  /* overkill? nah. */


     unsigned char  priority,
                    type,
                    running,
                    trace,
                    locked;
     string objname;

     /* need to include stuff like Current Object Glyph. This has to be
        in some sort of glyph-caching scheme, that is, when the
        object setglyph()'s, it needs to set a pointer to a bitmap
        object right here that is able to be drawn without searching
        through a name tree or something or reloading the thing from
        disk.

          Considerations:

        How do we let the glyph-cacher know that we're done with a
        glyph and it can be garbage-collected?
           - idea, glyph_garbagecollect() throws out pages which
             it doesn't find in use in any object's glyph*. call this
             function after battles, changing maps, etc.

        What about animated objects, or is that handled by some other
        function calling setglyph?
           - yes, some animation bit must call setglyph with words.
         */

     BITMAP * glyph;

     void init(),
          loadfromfile(string filename),
          run( int ticks );

     invoke_t invokemethod(string name);

     lob(string fname);
};

/* Find a place for these: */

void zeroize(unsigned short *, int);

void debug(string s);

string load_fopen ( string filename );

unsigned short correct(int d);
signed short makesigned(unsigned short d);

void send (string objname, string method);

void global_error ( string );
void step_pause();

#define isprinting(c) (((c) > 32) && ((c) < 255))

#endif
