/* Ludus and related source is distributed under the terms of the Ludus
   Source License, which can be found in the file COPYING in this or
   parent directories. */

// ---=[ LASM ]=-----------------------------------------------------------
//                  New C++ Version of the Ludus Assembler
//                          For Real Men(TM)
// ----------------------------------------------=[ lasm.cc ]=-------------

/* This code sort of hacky and untested. It should work fine on proper
   lasm code, but may fail on illegal code (without giving proper
   error messages, or indeed noticing the error at all). This is mainly
   because I am lazy and this is going to eventually be a backend for
   a more handsome language/compiler. */

#include <iostream.h>
#include <stdio.h>
#include <string>
#include <stdlib.h>
#include <fstream.h>

#ifndef isnum
#define isnum(c)  ((c)>='0' && (c)<='9')
#endif

#define IFBYTE(c) ((c)?(16)  :(  0))

#define isprinting(c) (((c) >= 32) && ((c) < 255))

#define uint  unsigned int
#define uchar unsigned char

#define MAX_ARGS 5
#define MAX_OBJTYPE 9

#define LABEL_UNDEFINED (-1)

enum { OPCODE, LVALUE, RVALUE, LABEL, METHOD, KEYWORD, EOFT,
       RAWNUM, RAWSTRING, };
 enum { STRING, NUMBER, POINTER, VARIABLE, REGISTER, RPOINTER,
                    ENCODED, // if I was able to encode it
                             // unconditionally. 
};

int LINENUM=0,OBJNAME_SET=0,OBJTYPE_SET=0,OUTHEADER=0;

#define NULLS ((string)"")

#define EOFTOKEN ((token){EOFT,NULLS})

struct injection {
     int data;
     injection * next;
};

struct symbol {
     string name;
     int location;
     injection * dests;
     symbol * left, * right;
};

struct stringz {
     int whence,
         location;
     stringz * next;
};

struct rvalue {
     char subtype;
     char data[3];
     char byte;
};

struct lvalue {
     char subtype;
     char data[3];
     char byte;
};

struct token {
     int type;
     string s;
     int d;
     rvalue r;
     lvalue l;
};

enum codes_t { C_NULL, C_WORD, C_LVALUE, C_VALUE, };

struct opcode {
     string name;
     codes_t args[MAX_ARGS];
     unsigned char data;
};

struct method {
     string name;
     int loc;
};

template <class T>
struct element {
  T data;
  element<T> * next;
};

unsigned char header[] = {
     '*','O','B','J','*', 0 /* OBJTYPE header[5] */,
     0,0,0,0,0,0,0,0 /* OBJNAME header[6], zero-filled */,
     0,0 /* NUMMETHODS header[14,15] */,
};

#define HEADERLEN (sizeof (header))

void    readcodes(string fname, element<opcode> *& codehead),
        warning  (string),
        error    (string),
        debug    (string),
        addmethod(string,int),
        insertstring(string s, int loc),
        newdata  (string name, int val, string s),
        uselabel (string,int),
        addlabel (string,int),
        registerdata(string name, int offset),
        recurse_injection(symbol * t, int offset),
        printbins(string s),
        printbin (const char * s, int len),
        writestrings( string s, FILE *& whence ),
        writestring ( const char * s, int len, FILE *& whence ),
        usedata  (string s, int loc);

codes_t what_arg (string);

int     reserved (string),
        iskeyword(string),
        regcode  (string),
        binval   (uchar ),
        hexval   (uchar ),
        tom_atoi (const
                  char *);

uint    correct(int d);

token   gettoken(int, FILE *,int=0),
        maketoken(string,int,FILE *, int=0);

string  tokenstring(token),
        typestring(int),
        rcodestring(int d);

string  methodsegment,codesegment,datasegment;

symbol * labels    = NULL,
       * variables = NULL;

stringz * strings  = NULL;

element<method> * methods   = NULL;

int main (int argc, char ** argv) {
     // use stdin for processing.

     element<opcode> * codehead = NULL;

     readcodes("codes.txt", codehead);

     token code,temp;

     FILE * source = stdin,
          * dest   = stdout;

     if (argc >= 2)
          if (!(source = fopen(argv[1],"r"))) error((string)"Cannot open input file " + (string)argv[1] + (string) "!");
     if (argc >= 3)
          if (!(dest = fopen(argv[2],"wb"))) error((string)"Cannot open output file " + (string)argv[2] + (string) "!");

     do {
          code = gettoken(0,source);
          switch (code.type) {
          case EOFT:
//               debug("Reached end of parsing.");
               
/* ------------------------------------------------------ */
          break;
          case OPCODE: OUTHEADER=1;
{               // do opcode.
     element<opcode> * soda = codehead;
     while (soda) {
          if (soda->data.name == code.s) {
               int z=0;

               codesegment += "o";
               codesegment[codesegment.length()-1]=(char)soda->data.data;

               while (soda->data.args[z]) {
               int l = codesegment.length();
               switch (soda->data.args[z++]) {
               case C_WORD:
                    // used for labels.
                    temp=gettoken(OPCODE,source);
                    uselabel(temp.s,codesegment.length());
                    codesegment += "lb";
               break;
               case C_LVALUE:
                    // left-hand value; must be writeable.
                    temp = gettoken(LVALUE,source);
                    switch(temp.l.subtype) {
                    case ENCODED:
                         codesegment += "eee";
                         codesegment[l++] = (char)(temp.r.data[0] | IFBYTE(temp.r.byte));
                         codesegment[l++] = (char)temp.r.data[1];
                         codesegment[l++] = (char)temp.r.data[2];
                    break;
                    case VARIABLE:
                         codesegment += "vvv";
                         codesegment[l++] = (char)(1 | IFBYTE(temp.r.byte));
                         usedata(temp.s,l);
                    break;
                    case POINTER:
                         codesegment += "ppp";
                         codesegment[l++] = (char)(0 | IFBYTE(temp.r.byte));
               /* WARNING?? byte pointer just gives the high byte of the
                  address ... */
                         usedata(temp.s,l);
                    break;
                    default:
                         cout << "It's: " << temp.l.subtype << endl;
                         error("Encountered lvalue subcode I don't understand.");
                    break;
                    }
               break;
               case C_VALUE:
                    temp = gettoken(RVALUE,source);
                    switch (temp.r.subtype) {
                    case STRING:
                         insertstring (temp.s, l+1);
                         codesegment += "sss";
                         /* FIXME: What is code for string? What does
                            ADD [A] "Poop"
                            mean? replace first byte s with that...

                            It means store the address raw, ie: 0.
                         */
                         codesegment[l]=(char)(0 | IFBYTE(temp.r.byte));
/* WARNING? */
                    break;
                    case POINTER:
                         codesegment += "ppp";
                         codesegment[l++] = (char)(0 | IFBYTE(temp.r.byte));
                         usedata(temp.s,l);
                    break;
                    case VARIABLE:
                         codesegment += "vvv";
                         codesegment[l++] = (char)(1 | IFBYTE(temp.r.byte));
                         usedata(temp.s,l);
                    break;
                    case ENCODED:
                         codesegment += "eee";
                         codesegment[l++] = (char)(temp.r.data[0] | IFBYTE(temp.r.byte));
                         codesegment[l++] = (char)temp.r.data[1];
                         codesegment[l++] = (char)temp.r.data[2];
                    break;
                    default:
                         cout << "It's: " << temp.r.subtype << endl;
                         error("Encountered rvalue subcode I don't understand.");
                    break;
                    }
               break;
               default:
               error("Opcode data corrupted. (?)");
               }
               }
          break;
          }
     soda = soda->next;
     }
     if (!soda) error ((string)"Unknown opcode " + code.s + string("!"));
}
/* ------------------------------------------------------ */
          break;
          case LABEL: OUTHEADER=1;
               addlabel(code.s,codesegment.length());
          break;
          case METHOD: OUTHEADER=1;
               addmethod(code.s,codesegment.length());
          break;
          case KEYWORD:
               if (OUTHEADER) warning("Keyword outside of header.");
               if (code.s == "DATA") {
                    temp = gettoken(OPCODE, source);
                    string name = temp.s;
                    temp = gettoken(RAWNUM, source);
                    newdata(name,temp.d,"");
               } else if (code.s == "POINTER") {
                    temp = gettoken(OPCODE, source);
                    string name = temp.s;
                    temp = gettoken(RAWSTRING, source);
                    newdata(name,0,temp.s);
               } else if (code.s == "OBJTYPE") {
                    if (OBJTYPE_SET) {
                         string k="x";
                         k[0] = (char)('0'+header[5]);
                         warning((string)"OBJTYPE already previously set (to \"" + k+ (string)"\")!");
                    }
                    temp = gettoken(RAWNUM, source);
                    if (temp.d < 0 || temp.d > MAX_OBJTYPE) error("OBJTYPE not within range.");
                    header[5] = (unsigned char)temp.d & 255;
                    OBJTYPE_SET++;
               } else if (code.s == "OBJNAME") {
                    if (OBJNAME_SET) {
                         warning((string)"OBJNAME already previously set (to \"" + (string)(char*)&header[6] + (string)"\")!");
                    }
                    temp = gettoken(RVALUE ,source);
                    if (temp.r.subtype != STRING) error ((string)"OBJNAME keyword requires a quoted object name.");
                    strncpy((char*)&header[6],temp.s.c_str(),8);
                    OBJNAME_SET++;
               } else {
                    warning ((string)"Keyword " + code.s + (string)" is not implemented.");
               }
          break;
          default:
               error (string("Got type ") + tokenstring(code)
                    + string(" and don't know what to do with it."));
          }
     } while (code.type != EOFT);

     /* do_injections:

        go through our data structures for data and strings, and inject
        their addresses now that we know the length of the header and
        code segments.

     */

     stringz * zzz = strings;

     uint methods_count = 0;
     element<method> * grr = methods;
     while (grr) { methods_count++; grr=grr->next; }

     int offset = (methods_count<<2) + HEADERLEN + codesegment.length();
               /* 4 bytes each for namepointer, datapointer */

     uint data;
     string methodpointers;

     while (methods) {
          methodsegment += "mm"; /* names */
          data = correct(offset + datasegment.length());
          methodsegment[methodsegment.length()-2] = 255&(data>>16);
          methodsegment[methodsegment.length()-1] = 255&data;
          datasegment += methods->data.name + (string)"x";
          datasegment[datasegment.length()-1] = (char)'\x00';
          methodpointers += "mm"; /* pointers */
          data = correct(HEADERLEN + (methods_count<<2) + methods->data.loc);
          methodpointers[methodpointers.length()-2] = 255&(data>>16);
          methodpointers[methodpointers.length()-1] = 255&data;
     methods = methods->next;
     }

     data = correct(methods_count);
     header[14] = 255&(data>>16);
     header[15] = 255&data;

     while (zzz) {
          data = correct(offset + zzz->location);
          codesegment[zzz->whence] = 255&(data>>8);
          codesegment[1+ zzz->whence] = 255&data;
     zzz = zzz->next;
     }

     recurse_injection(variables,offset);
     recurse_injection(labels,   offset - codesegment.length());
                                   /* stupid thinkos! */

     if (dest == stdout) {

               cout<<"HS:";
               printbin( (char *) header, HEADERLEN );
               printbins( methodsegment );
               printbins( methodpointers );
               printf("CS: 0x%04X\n",(unsigned int) (HEADERLEN + methodsegment.length() + methodpointers.length()));
               printbins( codesegment );
               printf("DS: 0x%04X\n",(uint) (codesegment.length() + HEADERLEN + methodsegment.length() + methodpointers.length()));
               printbins( datasegment );

     } else {
               writestring ( (char *) header, HEADERLEN ,dest);
               writestrings( methodsegment  ,dest );
               writestrings( methodpointers ,dest );
               writestrings( codesegment    ,dest );
               writestrings( datasegment    ,dest );
     }

exit(0);

}

void recurse_injection(symbol * t, int offset) {
     if (t) {

     injection * zzz = t->dests;
     uint data = correct(offset + t->location);

     while (zzz) {
          codesegment[zzz->data] = 255&(data>>8);
          codesegment[1+ zzz->data] = 255&data;
     zzz = zzz->next;
     }

     recurse_injection(t->left,offset);
     recurse_injection(t->right,offset);
     }
}

void addlabel(string s, int loc) {
     symbol ** poo = &labels;
     while (*poo)
          if (s == (*poo)->name) {
               if ((*poo)->location == LABEL_UNDEFINED) {
               (*poo)->location = loc; /* found forward-ref'd label */
               return;
               } else error(string("Label ") + s + (string) " redeclared.");
          }
          else if (s < (*poo)->name) poo = &(*poo)->left;
                                else poo = &(*poo)->right;
     *poo = new symbol((symbol){s,loc}); /* filled NULL */
}

void uselabel(string s, int loc) {
     symbol ** poo = &labels;
     while (*poo)
          if (s == (*poo)->name) {
               /* found it. */
               (*poo)->dests =
                 new injection((injection){loc,(*poo)->dests});
               return;
          }
          else if (s < (*poo)->name) poo = &(*poo)->left;
                                else poo = &(*poo)->right;

     /* at this point we haven't found it, but we can't give up because
        forward references of labels are allowed. Add it to the label
        tree with a loc of LABEL_UNDEFINED and hope it gets declared
        later. */
     *poo = new symbol((symbol){s,LABEL_UNDEFINED,
          new injection((injection){loc,NULL}) });
                                        /* remainder filled NULL */
}

void addmethod(string s, int loc) {
     /* Does not check for duplicate defs! */  /***** FIXME *****/
     methods = new
     element<method>((element<method>){{s,loc},methods});
}

void insertstring(string s, int loc) {
     /* Insert string s in the string segment, then register 'loc' as
        an insertion point for this string. */

//     cout << "insertstring(" << s << ", " << loc << ");\n";

     strings = new stringz
     ((stringz) { loc, datasegment.length(), strings } );

     datasegment += s;
     datasegment += "x";
     datasegment[datasegment.length()-1]=(char)0;
}

void newdata(string name, int val, string s) {
     /* Make room for a data word called name */
     int offset = datasegment.length();
     if (s=="") {
          // inserting a number variable.
          val = correct(val);
          datasegment += "xx";
          datasegment[offset] = (char) ((val >> 8)&255);
          datasegment[offset+1] = (char) (val&255);
     } else {
          // inserting a string variable.
           datasegment += s;
           datasegment += "x";
           datasegment[datasegment.length()-1] = '\0';
     }

     registerdata(name, offset);
}

void registerdata(string name, int offset) {
     /* put a data variable called 'name' in the registry with an
        offset in the data segment of offset. */

//     debug(string("Registering data called ") + name);

     symbol ** poo = &variables;
     while (*poo)
          if (name == (*poo)->name) error(string("Data ") + name + (string) " redeclared.");
          else if (name < (*poo)->name) poo = &(*poo)->left;
                                   else poo = &(*poo)->right;
     *poo = new symbol((symbol){name,offset}); /* filled NULL */
}

void usedata(string s, int loc) {
     /* use an existing data variable -- just registers 'loc' as an
        insertion point for the string. */
     symbol * moop = variables;
     while (moop)
          if (moop->name == s) {
               moop->dests = new injection((injection){loc,moop->dests});
               return;
          } else if (s < moop->name) moop = moop->left;
            else                     moop = moop->right;
     error((string)"Unrecognized variable '" + s + string("'!"));
}


void readcodes(string fname, element<opcode> *& codehead) {
     ifstream innie(fname.c_str());
     if (innie) {
          string codename;
          int codenum;
          while (innie >> codenum >> codename) {
               codehead = new
                 element<opcode>((element<opcode>)
                    {{ codename,
                       {(codes_t)0,
                        (codes_t)0,
                        (codes_t)0,
                        (codes_t)0,
                        (codes_t)0,
                        },codenum },codehead });
               int zz = 0;
               while ( zz < MAX_ARGS  && innie >> codename && codename != ".") {
                    codehead->data.args[zz++] = what_arg(codename);
               }
          }
     } else {
          cerr << "Couldn't open opcode file " << fname << ".\n";
          exit (-1);
     }
}

codes_t what_arg(string codename) {
     switch (codename[0] | 32) {
        case 'w': return C_WORD;
        case 'v': return C_VALUE;
        case 'l': return C_LVALUE;
        default : cerr << "I can't understand \"" << codename << "\" as an opcode argument.\n";
     }
}

token gettoken(int kind, FILE * infile, int isbyte = 0) {
     string s;
     int c;
     int inquotes=0, escaped=0, // ah, a finite state machine? =)
         incomment=0;

     yepyep:;

     while ((c = getc(infile)) != EOF) { // prespace
          if (c == '\n') { LINENUM++; incomment=0; }
          if (!(c == ' ' || c == '\t' || c == '\r' || c=='\n')) break;
     }

     if (c != EOF) {
     do {
       if (c == '\n') LINENUM++;

       if ((!inquotes) && (c == '\'')) {
          while ((c=getc(infile)) != EOF) if (c== '\n') { LINENUM++; goto yepyep; }
          if (c == EOF) return EOFTOKEN;
       }

          if ((!inquotes) && (c == ' ' || c == '\t' || c == '\r' || c=='\n')) break;
          if (c == '\\') if (!escaped) { escaped=1; continue; }
          if ((!escaped) && c == '\"') if (!(inquotes^=1)) break;
          s+=(char)c; // include starting " but not final
          escaped=0;

              /* It is my opinion that C needs a way to break out of loops
                 more than one level deep. My proposition:
              
                 break(continue(break(break)));
              
                 break twice, continue, then break.
              */

     } while ((c = getc(infile)) != EOF);
     } else return EOFTOKEN;

//     cout << "(" << s; 
     token dummy = maketoken(s,kind,infile,isbyte);
     if (kind && dummy.type != kind) {
//          cout << ")\n";
          error(tokenstring(dummy) + " is not of expected type " + typestring(kind));
          } //else cout << " :: " << tokenstring(dummy) << ")\n";
     return dummy;
}

token maketoken(string s,int kind,FILE * infile, int isbyte=0) {
token dummy;
dummy.type = EOFT; // error

//     printf("Maketoken(%s,%s);\n", s.c_str(), typestring(kind).c_str());

if (isbyte) {
     dummy.r.byte =
     dummy.l.byte = 1;
} else {
     dummy.r.byte =
     dummy.l.byte = 0;
}

       if (s[0]  != '\"') for (uint x=0;x<s.length();x++) if (isalpha(s[x]))
                         s[x] &= ~32;

     if (s == "BYTE") {
          /* got flag for 'byte' */
          if (!kind || (kind == RVALUE || kind == LVALUE)) {
               return gettoken(kind,infile,1);
          } else
          error ("Misplaced keyword 'BYTE'.");
     }

       if (s[0] == '!') {
           dummy.s = (char*)(s.c_str()+1);
           dummy.type = METHOD;
           return dummy;
       } else if (s[0] == ':') {
           dummy.s = (char*)(s.c_str()+1);
           dummy.type = LABEL;
           return dummy;
       } else if (s[0] == '\"') {
           // I don't know what the insertion point would be.
           // So I have to return it as a string.
           dummy.s = (char*)(s.c_str()+1);

//           debug((string)"doing string \"" + dummy.s + (string)"\", kind == " + typestring(kind));
           if (kind && (kind == RAWSTRING) ) {
               dummy.type = RAWSTRING;
               return dummy;
           } else {
                dummy.type = RVALUE;
                dummy.r.subtype = STRING;
                return dummy;
           }
       } /* else if (isnum(s[0])) {
           int d = correct(atoi(s.c_str()));
           dummy.type = RVALUE;
           dummy.r.subtype = ENCODED;      // TOM == RETARDED!
           dummy.r.data[0] = 0 | IFBYTE(isbyte);
           dummy.r.data[1] = 255&(d>>8);
           dummy.r.data[2] = 255&d;
           return dummy;
       } */ else if (s[0] == '[') {
           s[s.length()-1]=0;
           int d = correct(regcode((string)(char*)(s.c_str()+1)));
           if (kind) dummy.type = kind; else dummy.type = RVALUE;
           dummy.r.subtype = dummy.l.subtype = ENCODED; /* ? */
           dummy.r.data[0] = dummy.l.data[0] = 2 | IFBYTE(isbyte);
           dummy.r.data[1] = dummy.l.data[1] = 255&(d>>8);
           dummy.r.data[2] = dummy.l.data[2] = 255&d;
           return dummy;
       } else if (s[0] == '%') {
           dummy.s = (char*)(s.c_str()+1);
           if (kind) {
           dummy.type = kind; } else dummy.type = RVALUE;
           dummy.r.subtype = 
           dummy.l.subtype = VARIABLE;
           if (kind && !(kind == RVALUE || kind == LVALUE)) error("Can't convert"
              " expression \"" + s + "\"!");
           return dummy;
       } else if (s[0] == '$') {
           dummy.s = (char*)(s.c_str()+1);
           if (kind) {
           dummy.type = kind; } else dummy.type = RVALUE;
           dummy.r.subtype = 
           dummy.l.subtype = POINTER;
           if (kind && !(kind == RVALUE || kind == LVALUE)) error("Can't convert"
              " expression \"" + s + "\"!");
           return dummy;
       } else if (s[0] == '-' || ((unsigned char)s[0] >= (unsigned char)'0'
              &&  (unsigned char)s[0] <= (unsigned char)'9')) {

//          printf ("Kind: %s\n", typestring(kind).c_str());

           if (kind == RAWNUM) {
               dummy.type = RAWNUM;
               dummy.d = tom_atoi(s.c_str());
               return dummy;
           } else { // make it an rvalue
               int d = correct(tom_atoi(s.c_str()));

               dummy.type = RVALUE;
               dummy.r.subtype = ENCODED;
               dummy.r.data[0] = 0 | IFBYTE(isbyte);
               dummy.r.data[1] = (d >> 8)&255;
               dummy.r.data[2] = (d&255);
               return dummy;
           }

       } /* else { cout << "missed all tests...\n";} */

       if (kind && (kind == RVALUE || kind == LVALUE)) {
          // epecting an rvalue or lvalue by this point.
          // we've covered all the other bases, so we'll try for a
          // register, and fail if we don't find one.

          dummy.type = kind;
          dummy.r.subtype = dummy.l.subtype = ENCODED;
          int d = regcode(s);

               dummy.r.data[0] = 3 | IFBYTE(isbyte);
               dummy.r.data[1] = d&255;
               dummy.r.data[2] = 0;     /* stupid typos! */
               dummy.l.data[0] = 3 | IFBYTE(isbyte);
               dummy.l.data[1] = d&255;
               dummy.l.data[2] = 0;

          return dummy;
       }

      // don't care. Probably OPCODE/KEYWORD/LABEL/METHOD
         dummy.s = s;
         if (iskeyword(s)) 
                         /* if in keyword list... */
           dummy.type = KEYWORD;
         else
           dummy.type = OPCODE;
return dummy;
}

int iskeyword(string s) { return (s=="DATA" || s=="OBJTYPE" || s=="OBJNAME" || s == "POINTER"); }

string tokenstring(token d) {
     if (d.type == RVALUE) {
          return ((string)"RVALUE->" + rcodestring(d.r.subtype));
     }
     return typestring(d.type);
}

#define caseret(c) case c: return #c            // ooh slick.
string typestring(int d) {
     switch (d) {
        caseret(OPCODE);
        caseret(LVALUE);
        caseret(RVALUE);
        caseret(LABEL);
        caseret(METHOD);
        caseret(KEYWORD);
        caseret(EOFT);
        caseret(RAWNUM);
        caseret(RAWSTRING);
     } return "??????";
}
string rcodestring(int d) {
     switch (d) {
          caseret(STRING);
          caseret(NUMBER);
          caseret(POINTER);
          caseret(VARIABLE);
          caseret(REGISTER);
          caseret(RPOINTER);
          caseret(ENCODED);
     } return "??????";
}
int regcode(string s) {
     if (s.length()==1) {
          switch(s[0]) {
          case 'A': return 0;
          case 'B': return 1;
          case 'C': return 2;
          case 'D': return 3;
          case 'R': return 4;
          default:
          error(string("'") + s + (string) "' is not a register.\n   (Only registers may appear as a value\n    without an identifying %, $, [, etc.)\n");
          }
     }
     if (s[0] == 'I') {
          if (s.length() == 6 || s.length() == 7) {
          int d = atoi((char*)s.c_str()+5);
          if (d >= 1 && d<=10) return 0x10 + (d-1);
          }
     } else if (s[0]=='O') {
          if (s.length() == 7 || s.length() == 8) {
          int d = atoi((char*)s.c_str()+6);
          if (d >= 1 && d<=10) return 0x20 + (d-1);
          }
     }
          error(string("'") + s + (string) "' is not a register.");
}

void error(string e) {
     cout << "line " << LINENUM << " ERROR: " << e << endl;
     exit(-1);
}
void warning(string e) {
     cout << "line " << LINENUM << " warning: " << e << endl;
}
void debug(string e) {
     cout << "[DEBUG] line " << LINENUM << ": " << e << endl;
}

int reserved(string s) {
     // checks to see if s is a reserved keyword. should be all caps coming
     // in.
     return (s == "DATA" || s == "OBJNAME" || s == "OBJTYPE" || s=="POINTER");
}

uint correct(int d) {
     // make low 16 bits correct signed int //
     uint z =   d &  32767;
     if (d < 0) z |= 32768;
     return z;
}

void printbins ( string z ) { printbin (z.c_str(),z.length()); }

void printbin ( const char * s, int len )  {
     string aside;
     int waiting=0;
     int x=0;
     for (;;) {
     if (!(x<len)) waiting=1;
          if (!(x%16)) {
               cout << "    " << aside << endl;
               aside = "";
               if (waiting) break;
          } else if (!(x%8)) cout << " ";
          if (waiting) cout << "   ";
               else {printf("%02X ", (unsigned char)s[x]);
          if (isprinting((unsigned char)s[x])) aside += s[x] ; //+ string(" " );
                         else   aside +=        string(".");
                    }
     x++;
     }
     cout << endl;
}

void writestrings( string s, FILE *& whence ) {
     writestring ( s.c_str(), s.length(), whence );
}

void writestring ( const char * s, int len, FILE *& whence ) {
     for (int x=0;x<len;x++)
          fputc(s[x],whence);
}

int tom_atoi( const char * s ) {
     if (s[0] == '0') {
          if ((s[1]|32) == 'x') {         /* HEX */
               int x=2,val=0;
               while (s[x]) {
//                 cout << s[x] << endl;
                 val = (val<<4) | hexval((unsigned char)s[x++]);
               }
               return val;
          } else if ((s[1]|32) == 'n') {  /* BIN */
               int x=2,val=0;
               while (s[x]) {
//                                cout << s[x] << endl;
                 val = (val<<1) | binval((unsigned char)s[x++]);
               }
               return val;
          } else if (!s[1])
               return 0;
          else {                          /* OCT */

               error("Who uses *octal* ?");
          }
     } else return atoi(s);
     return 0 ;
}

int binval(unsigned char digit) {
//    cout << "doing binval(" << digit << ");\n";
    if (digit == '1') return 1;
    else if (digit == '0') return 0;
    else error ("Garbage in binary constant.");
    return 0;
}

int hexval(unsigned char digit) {
    if (digit >= (unsigned char)'0' && digit <= (unsigned char)'9')
          return digit-(unsigned char)'0';
    else {
     digit |= 32;
     if (digit >= (unsigned char)'a'
      && digit <= (unsigned char)'f') return 10+(digit-(unsigned char)'a');
     else error ("Garbage in hex constant...");
    }
    return 0;
}


