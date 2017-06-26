 
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

void zeroize(unsigned short * a, int b) { while (b--) a[b]=0; }

#define deb(c) << #c ": " << c << endl

void lob :: run ( int ticks ) {
     if (!running) return;

//     signed short x;

     while (ticks--) {

if (trace) {
     cout << "Ticks left: " << ticks << "  at IP: " << IP << endl;
     step_pause();

     printbins(program);
     printregs();
}
          switch((unsigned char)program[IP++]) {
          case O_STOP:
               running = 0;
               locked = 0; /* Bad form to stay locked, I say! */
               return;
          break;
          case O_LOCK:
               locked = 1;
          break;
          case O_UNLOCK:
               locked = 0;
          break;
          case O_HCF:
               cout << "Object " << objname << " terminated the engine.\n";
               exit(getvalue(IP));
          break;
          case O_JUMP:
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
          break;
          case O_RJUMP:
               { signed short x = makesigned(getword(IP));
                 IP += x-1;
                 if (! ( (IP >= 14) && ((uint)IP < program.length()) ) )
                    error("RJUMP puts IP out of program.");
               }
          break;
          case O_INC:
               setlvalue(IP, correct(getvalue(IP)+1));
               IP += 3;
          break;
          case O_DEC:
               setlvalue(IP, correct(getvalue(IP)-1));
               IP += 3;
          break;
          case O_ADD:
               setlvalue(IP, correct(getvalue(IP)+getvalue(IP+3)) );
               IP += 6;
          break;
          case O_SUB:
               setlvalue(IP, correct(getvalue(IP)-getvalue(IP+3)) );
               IP += 6;
          break;
          case O_MUL:
               setlvalue(IP, correct(getvalue(IP)*getvalue(IP+3)) );
               IP += 6;
          break;
          case O_DIV:
              { int d = getvalue(IP),
                    s = getvalue(IP+3);
               setlvalue(IP, correct(d/s));
               regs[4] = d % s; /* reg R holds remainder */
               IP += 6;
              }
          break;
          case O_FUNCTION:
              {
                uint d = getvalue(IP);
                if (d < program.length());
                if (3==callfunction((char *)(program.c_str() + d))) {
                    cout << "[couldn't find func]: " << objname <<
                                                  "." << ((char*)(program.c_str()+d))
                                                  << endl;
                }
               IP += 3;
              }
          break;
          case O_CMP:
              { int d = getvalue(IP),
                    s = getvalue(IP+3);
                if (trace) cout << "Compare(" << s << ", " << d << ");\n";
                if (s == d) regs[4] = 0;
           else if (d >  s) regs[4] = 1;
           else             regs[4] = 2;
                IP += 6;
              }
          break;
          case O_CMPS:
              { int d = makesigned(getvalue(IP)),
                    s = makesigned(getvalue(IP+3));
                if (s == d) regs[4] = 0;
           else if (d >  s) regs[4] = 1;
           else             regs[4] = 2;
                IP += 6;
              }
          break;
          case O_COPY:
              setlvalue(IP, getvalue(IP+3));
              IP += 6;
          break;
          case O_JL:
              if (regs[4] == 2)
              {
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
              } else IP += 2;
          break;
          case O_JLE:
              if (regs[4] == 2 || !regs[4])
              {
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
              } else IP += 2;
          break;
          case O_JG:
              if (regs[4] == 1)
              {
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
              } else IP += 2;
          break;
          case O_JGE:
              if (regs[4] == 1 || !regs[4])
              {
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
              } else IP += 2;
          break;
          case O_JE:
              if (!regs[4])
              {
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
              } else IP += 2;
          break;
          case O_JNE:
              if (regs[4])
              {
               IP = getword(IP);
               if (!(IP < program.length())) error ("JUMP out of program range.");
              } else IP += 2;
          break;
          case O_SEND:
              { uint d = getvalue(IP),
                    s = getvalue(IP+3);
                if (d < program.length() && s < program.length())
                     send((char*)(program.c_str()+d),(char*)(program.c_str()+s));
                else error("Pointer out of program space.");
                IP += 6;
              }
          break;
          case O_RAND:
               debug("RAND Opcode not ready...");
               IP += 9;
          break;
          default:
               cout << "Unknown Opcode: " << (unsigned char)program[IP-1] << endl;
//               error("Unknown opcode!");
               break;
          } // switch opcode
     } // while ticks--
}


lob :: lob ( string obn ) {
     valfuncs[0] = getword;
     valfuncs[1] = getbyte;

     loadfromfile(obn + (string) ".lob");
     init();
}

void lob :: init ( ) {
     zeroize(regs,5);
     zeroize(outarg,10);
     zeroize( inarg,10);

     priority = trace = running = locked = 0;
}

invoke_t lob :: invokemethod(string name) {
     if (locked) return INVOKE_LOCKED;
     
     method * m = methods;

     while (m) {
          if (m->name == name) {
               IP = m->loc;
               running = 1; /* duh */
               run(1);
               /* (always run at least one tick -- if the first opcode is
                   LOCK, we want to run that to prevent it from being
                   interrupted later!)
               */
               return INVOKE_OK;
          }
     m = m->next;
     }
     return INVOKE_NOMETHOD;
}

#define VAL_RAW 0
#define VAL_POINTER 1
#define VAL_REGPOINTER 2
#define VAL_REGISTER 3

unsigned short lob :: getvalue(int start) {
     unsigned char dood = program[start];
     int size = dood>>4;
     if (size > 2 || size < 0) {
          cout deb(start) << "[getvalue] Corrupted value byte: " << (int)(unsigned char) program[start] << endl;
          error("Corrupt object.");
     }
    switch (dood&15) {
    case VAL_RAW: return valfuncs[size](start+1); break;

    case VAL_POINTER:
         {
          unsigned short s = getword(start+1);
          if (s< program.length())
          return valfuncs[size](s);
          else error("Pointer out of program space!");
         }
    break;
    case VAL_REGPOINTER:
         {
          unsigned short s = *register_addr((unsigned char)program[(start+1)]);
          if (s < program.length())
          return valfuncs[size](s);
          else error("Regpointer is out of program space!");
         }
    break;
    case VAL_REGISTER:
           return (*register_addr((unsigned char)program[(start+1)])) & (size?0xFF:0xFFFF); /* Bytesize hack. Who just wants the top?? */
    break;
    default:
          cout deb(start) << "[getvalue] Corrupted value byte: " << (int)(unsigned char) program[start] << endl;
          error("Corrupt object.");
    break;
    }
    return 0; /* -Wall appeaser */
}

unsigned short * lob :: register_addr (unsigned char x) {
     switch (x>>4) {
     case 0:
          if ((x&15) <= 4) return &regs[x&15];
          else error("Corrupt object -- register code");
     break;
     case 1:
          if ((x&15) <= 9) return &inarg[x&15];
          else error("Corrupt object -- register code");
     break;
     case 2:
          if ((x&15) <= 9) return &outarg[x&15];
          else error("Corrupt object -- register code");
     break;
     default:
          cout << "[register_addr] Corrupted register byte: " << x<< endl;
          error("Corrupt object.");
     }
     return 0; /* -Wall */
}

void lob :: setlvalue(int start, unsigned short value) {
     unsigned char dood = program[start];
     int size = dood>>4;
     if (size > 2 || size < 0) {
          cout deb(start) << "[setlvalue] Corrupted value byte: " << (int)(unsigned char) program[start] << endl;
          error("Corrupt object.");
     }

     switch (dood & 15) {
     case VAL_RAW:
          cout deb(start) << "[setlvalue] Cannot setlvalue to a raw value (rvalue!)\n";
          error("Corrupt object.");
     break;
     case VAL_POINTER:
          { uint s = getword(start+1);
            if (s < program.length()-(size^1)) { /* how's that hack? =) */
            if (size) {
                 program[s] = value & 255;
            } else {
                 program[s] = value >> 8;
                 program[s+1] = value & 255;
            }
            } else error ("Pointer is out of program in rvalue!");
          }
     break;
     case VAL_REGPOINTER:
          {
            uint s = * register_addr((unsigned char)program[(start+1)]);
            if (s < program.length()-(size^1)) { /* how's that hack? =) */
            if (size) {
                 program[s] = value & 255;
            } else {
                 program[s] = value >> 8;
                 program[s+1] = value & 255;
            }
            } else error ("Reg pointer is out of program in rvalue!");
          }
     break;
     case VAL_REGISTER:
         {
          unsigned short * m =
          register_addr((unsigned char)program[(start+1)]);
          if (size) *m = ((*m)&0xFF00)|(value&255);
             else   *m = value;
         }
     break;
     default:
          cout deb(start) << "[setlvalue] Corrupted value byte: " << (int)(unsigned char) program[start] << endl;
          error("Corrupt object.");
     break;
     }
}
unsigned short lob :: getbyte(int start) {
    return (unsigned char)program[start];
}
unsigned short lob :: getword(int start) {
    return (((unsigned char)program[start])<<8)
    |(((unsigned char)program[start+1]));
}

void lob :: loadfromfile( string filename ) {
     debug ((string)"(( Opening from file \"" + filename + (string)"\" ))");

     // later this will be opened from a packfile.
     program = load_fopen( filename );

     if (strncmp(program.c_str(),"*OBJ*",5)) error ("Object file missing *OBJ* header.");
     if (program.length() < 16) error ("Object file looks truncated.");
     /* Well, we'll assume it's a legitimate object file then. */
     
     type = program[5];
     objname = (char *)(program.c_str() + 6);

     int num_methods = (program[14]<<8)
                     |  program[15];

#define pop(c) ((unsigned char)program[c])

     for (int m=0;m<num_methods;m++)
{ 
            register_method(  (char*)(program.c_str()+
                          ((pop(16 + (m<<1))<<8)
                          | pop(17 + (m<<1)))
          ),
                           (pop(16+(num_methods<<1)+(m<<1))<<8)
                          | pop(17+(num_methods<<1)+(m<<1))
                           );
}
     cout /* deb((int)type) */
          deb(objname)
          /* deb(num_methods) */

          ;
}

string load_fopen ( string filename ) {
     FILE * inways;

     if (!(inways = fopen(filename.c_str(),"rb") )) global_error((string)"Can't open " + filename + (string)".");

     int c;
     string s;
     while ( (c=getc(inways)) != EOF ) {
          s+=(char)c;
     }
     fclose (inways);
     return s;
}

void lob :: register_method ( const char * name, int address) {
/*
     cout deb(name)
          deb(address);
*/
     methods = new method ((method)
            {(string)name,address,methods});
}

void lob :: printregs () {
     for (int z=0;z<5;z++)
          cout << regs[z] << " ";
     cout << endl;
}

#undef deb(c)

/* HELPER FUNCTIONS, PLEASE MOVE: */

void send (string objname, string method) {
     debug ((string) "Sent: " + objname + (string)"." + method);
}

signed short makesigned(unsigned short d) {
     if (d & 32768) return - (d&32767);
               else return   (d&32767);
}

unsigned short correct(int d) {
     // make low 16 bits correct signed int //
     unsigned short z =   d &  32767;
     if (d < 0) z |= 32768;
     return z;
}


void debug (string s) {
     cout << "[Debug]: " << s << endl;
}

void lob :: error (string s) {
     cout << "[" << objname << " ERROR]: " << s << endl
          << "at IP: " << IP << endl;
     exit(-1);
}

void global_error (string s) {
     cout << "[global ERROR]: " << s << endl;
     exit(-1);
}

void lob ::printbins ( string z ) { printbin (z.c_str(),z.length()); }

void lob ::printbin ( const char * s, int len )  {
     string aside;
     int waiting=0;
     int x=0;
     for (;;) {
     if (!(x<len)) waiting=1;
          if (x && !(x%16)) {
               cout << "    " << aside << endl;
               aside = "";
               if (waiting) break;
          } else if (x && !(x%8)) cout << " ";
          if (waiting) cout << "   ";
               else { if (IP==(unsigned int)x) printf("%02X<", (unsigned char)s[x]);
                                          else printf("%02X ", (unsigned char)s[x]);
          if (isprinting((unsigned char)s[x])) aside += s[x] ; //+ string(" " );
                         else   aside +=        string(".");
                    }
     x++;
     }
     cout << endl;
}

void step_pause() {
     cout << "/pause/\n";
     getch();
}


