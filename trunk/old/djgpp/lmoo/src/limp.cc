/* limp is distributed under the Gnu Public License. See
    http://gnu.org/copyleft/gpl.html
   for licensing details. */

#define LIMP_VERSION "97.0"

#include <stdlib.h>
#include <stdio.h>
#include <string>
#include "limp.h"
#include "global.h"
#include <setjmp.h>
     
/* limp is a programming language posessing the following qualities:
    - slow
    - simple
    - utilitarian
    - recursive
    - interpreted
*/

 void (*print_limp_error)(string) = NULL;
 void (*print_limp_debug)(string) = NULL;
 int  (*limp_interrupt)() = NULL;

limp_funcstuff limp_internals[] = {
     {"limp-version", 0},
     {"find", 0},
     {"word", 0},
     {"wmatch", 0},
     {"cut", 0},
     {"eval", 0},
     {"throw", 0},
     {"verbose", 0},
     {"set", 0},
     {"shadow", 0},
     {"scope", 0},
     {"quote", 0},
     {"debug", 0},
     {"while", 0},
     {"dowhile", 0}
};

/* jmp_buf error_panic; */

 int LIMP_VERBOSITY=0;

#define EVALtoken(c) evaltoken(c)
/*
 ({                                          \
                    string _c = (c);                             \
                    print_limp_debug((string)"Eval(" + _c+string(")"));      \
                    string _z =evaltoken(_c);                    \
                    print_limp_debug((string)"== \"" + _z + string("\"")); \
                    _z; })
*/
#define NEXTtoken(c) nexttoken(c)
#define RET_ERROR(msg) \
          return "\x08THROW " msg;
//     longjmp(error_panic,1);

#define ISret(c) (c.substr(0,6) == "\x08THROW")
#define CHECKret(c) if ( ISret(c) ) return c;
#define SAFEtoken(m,z) string m = evaltoken(nexttoken(z)); \
                       CHECKret(m)

#define NEWSCOPE pushscope(limp_ROOT, new limp_node(" DuMMy`**",".",NULL,NULL))

limp_node_stack   * limp_ROOT = NULL;
limp_func_node    * limp_FUNC_ROOT;
limp_node_stack   * limp_global_scope = NULL;
limp_userfun_node * limp_userfun_root = NULL;

void limp_init() {
  NEWSCOPE;
  limp_global_scope = limp_ROOT;
  LIMP_VERBOSITY = 0;
  limp_FUNC_ROOT = new limp_func_node(" D*u\"m`m(y(", NULL);
  limp_regfuncstuffs(limp_internals); /* internal functions */
  print_limp_debug = NULL;
  print_limp_error = NULL;
}

#define oktok(c) c

void pushscope(limp_node_stack * & rut, limp_node * sfoot) {
     // adds a scope to the stack
     rut = new limp_node_stack(rut,sfoot);
}

limp_node_stack::
limp_node_stack(limp_node_stack*a,limp_node*b) {next=a;node=b;}

int popscope(limp_node_stack * & rut) {
     // pops the top scope if it is not the last one. Frees data.
     // returns 1 if it popped anything.
     if (rut && rut->next) {
          limp_node_stack * smut = rut;
          rut = rut->next;
          delete smut;
          return 1;
     } else return 0;
}

string limp_node_stack :: get ( string key ) {
     if (!node) return "NULL";
     limp_node_stack * zxcv = this;
     while (zxcv) {
          string oz = zxcv->node->get(key);
          if (oz != "`**NULL") return oz;
          zxcv = zxcv->next;
     }
     return "NULL";
}

void limp_regfunc(string name, ext_limp_token (*f)(stringqueue&)) {
  limp_FUNC_ROOT->add_func(name,f);
}

void limp_setval(string key, string value) {
     /* gotta find the closest shadowing of this variable and use that.
        annoyingly, and perhaps stupidly, we first 'get' the variable
        then 'set' if we didn't get `**NULL back. */
     limp_node_stack * zxcv = limp_ROOT;
     while (zxcv) {
          string oz = zxcv->node->get(key);
          if (oz != "`**NULL") {
               zxcv->node->set(key,value);
               return;
          }
          zxcv = zxcv->next;
     }
     limp_ROOT->node->set(key,value);
}

void limp_global_set(string k,string v) {
     limp_global_scope->node->set(k,v);
}

string limp_getval(string key) {
  return limp_ROOT->get(key);
}

int istrue(string in) {
  return (eval_limp(in) == "YES");
}

string eval_limp(string in) {
  /* Must *NOT* be called recursively, because of setjmp: */
//  print_limp_debug((string)"eval(~0"+in+(string)"~e)");
  limp_clearinterrupt();

#if 0
  if (setjmp (error_panic)) {
     while (popscope(limp_ROOT)); /* pop all scopes but last one */
     return "(ERROR)";
  }
#endif

  string evame = losewhites(in);
//  print_limp_debug((string)"eval(~0"+evame+(string)"~e)");

  if (evame == "") return "";
  string outs = evaltoken(evame);  



  if (outs.substr(0,6) == "\x08THROW") {
     print_limp_error((string)"Unhandled exception: "
          + outs.substr(7,outs.length()-7));
  return outs.substr(7,outs.length()-7); }
  else
  return outs;
}

string evaltoken(string in) {
  /* tokens may not have pre and post spaces */

  string args;

  if (!in.length()) return "";

//     limp_debug((string)"~b[~G" + in + (string)"~b]");

  if(limp_checkinterrupt()){RET_ERROR("INTERRUPTED");}

  switch(in[0]) {
  case '\x08':
    { /* this is a \x08THROW type statement. This can happen when
         NEXTtoken has an error and wants to abort. Just return this
         to be caught by handle or CHECKret. */
          
         return in;
    }
  case '%':
    {     /* shortcut to dereference a variable */
//       limp_debug((string)"Shortcut-deref: ~g" + in.substr(1,in.length()-1));
       return limp_ROOT->get(in.substr(1,in.length()-1));
       break;
    }
  case '\'':
    {
          // This is a 'quoted string. Evaluation consists of
          // simply unquoting it.
          return in.substr(1,in.length()-1);
    }
  case '(':
    {
      string cmd;
      for (uint x=1;x<in.length();x++) {
	if (whitespc(in[x]) || in[x] == ')' || in[x] == '(') {
	  /* found end */
	  args = in.substr(x,in.length()-(x+1));
	  break;
	} else cmd += in[x];
      }

     if (!cmd.length()) {
//        print_limp_debug("~rWARNING~0: used null function ()");
        string b= NEXTtoken(args);
        CHECKret(b);
        if (b != "")
        return EVALtoken(b);
        else return "NO";
     } else if (cmd.length() ==1) {
      switch (cmd[0]) {
      case '#': /* evaluate but supress output */
     { string food;
     while ("" != (food = NEXTtoken(args))) {
       EVALtoken(food);
       CHECKret(food); }
      return ""; 
      break;
     }
      case ',': // comma operator evaluates all tokens sequentially
     {
       string food,ofood="NO";
     while ("" != (food = NEXTtoken(args))) {
       ofood = EVALtoken(food);
       CHECKret (ofood); }
     return ofood;
     break;
     }
      case '?': // conditional evaluation (? (condition) (do-if-not-NO)
                //                           (do-if-NO))
     {
       string food;
       if (EVALtoken(NEXTtoken(args)) != "NO") {
          SAFEtoken(foodz,args);
          food = foodz;
       } else {
          NEXTtoken(args); // discard
          SAFEtoken(foodz,args);
          food = foodz;
       }
//       print_limp_debug(food);
       return food;
     }
      case '<':
	{
       SAFEtoken(aa,args);
       SAFEtoken(bb,args);
       int a = atoi( aa  .c_str() );
       int b = atoi( bb  .c_str() );

       return oktok((a<b)?"YES":"NO");
	  break;
	}
      case '>':
	{
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);
       int a = atoi( aa.c_str() );
       int b = atoi( bb.c_str() );

       return oktok((a>b)?"YES":"NO");
	  break;
     }
      case '=': /* set variable */
     {
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);

      limp_setval(aa,bb);
      return bb;
      break;
     }
      case '%': /* dereference */
      {
       SAFEtoken(aa, args);
       return limp_ROOT->get(aa);
       break;
      }
      case '&': /* concatenate n strings */
	{ 
	  string food,out;
     while ("" != (food = NEXTtoken(args))) {
       food = EVALtoken(food);
       CHECKret(food);
	  out += food;
	}
     return out;
	}
	break;
      case '*': /* mult n numbers */
	{
	  string food; 
       int out=1;
     while ("" != (food = NEXTtoken(args))) {
       food = EVALtoken(food);
       CHECKret(food);
       out *= atoi(food.c_str());
	}
     return oktok(itos(out));
	}
	break;
      case '/': /* integer-divide a by b,c,d,e,f,g... */
	{
	  string food;
       SAFEtoken(dude,args);
       
       int out=atoi(dude.c_str());
     while ("" != (food = NEXTtoken(args))) {
       food = EVALtoken(food);
       CHECKret(food);
       int sp = atoi(food.c_str());
       if (sp) out /= sp;
       else return "NaN";
	}
     return itos(out);
	}
	break;
      case '-': /* subtract b,c,d,e,f,g... from a */
	{
	  string food;
          SAFEtoken(dude,args);
       int out=atoi(dude.c_str());
     while ("" != (food = NEXTtoken(args))) {
       food = EVALtoken(food);
       CHECKret(food);
       out -= atoi(food.c_str());
	}
     return oktok(itos(out));
	}
	break;
      case '+': /* add n numbers */
	{
	  string food; 
	  int out=0;
     while ("" != (food = NEXTtoken(args))) {
       food = EVALtoken(food);
       CHECKret(food);
	  out += atoi(food.c_str());
	}
     return oktok(itos(out));
	}
	break;
      default:
     limp_error((string)"Unknown operator in limp exp: " +cmd);
     RET_ERROR("UNKNOWNOP");
	break;
      }
      } else { // (multi-line:)
	if (cmd == "debug") {
       SAFEtoken(aa, args);
       limp_debug(aa);
       return oktok("NO"); /* FIXME: maybe this should return aa? */
     } else if (cmd == "while") {
       string condition = NEXTtoken(args);
       string body = NEXTtoken(args);

       string evalto = EVALtoken(condition);
       CHECKret(evalto)
       while ("NO" != evalto) {
         if(limp_checkinterrupt()){RET_ERROR("INTERRUPTED");}
         evalto = EVALtoken(body);
         CHECKret(evalto);
         evalto = EVALtoken(condition);
         CHECKret(evalto);
       }
       return "YES";
     } else if (cmd == "dowhile") {
       string condition = NEXTtoken(args);
       string body = NEXTtoken(args);
       string evalto;
       do {
         if(limp_checkinterrupt()){RET_ERROR("INTERRUPTED");}
         evalto = EVALtoken(body);
         CHECKret(evalto);
         evalto = EVALtoken(condition);
         CHECKret(evalto);
       } while ("NO" != evalto);
       return "YES";
     } else if (cmd == "set") { // like setq
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);
       limp_global_set(aa,bb);
       return bb;
     } else if (cmd == "shadow") {
       string yeah="uninitialized"; // for string&
       SAFEtoken(aa,args);
           limp_ROOT->node->set(aa,yeah);
       return aa;
     } else if (cmd == "scope") {
       NEWSCOPE;
       string retme = EVALtoken(NEXTtoken(args));
       popscope(limp_ROOT);
       CHECKret(retme); // after popping scope...
       return retme;
     } else if (cmd == "quote") {
       return  NEXTtoken(args);
     } else if (cmd == "==") {
       SAFEtoken(b,args); // = EVALtoken(NEXTtoken(args));
       SAFEtoken(c,args);
       return (b==c)?"YES":"NO";
     } else if (cmd == "throw") {
       SAFEtoken(aa,args);
       return string("\x08THROW ") + aa;
     } else if (cmd == "eval") {
       SAFEtoken(aa,args);
       aa = EVALtoken(aa);
       CHECKret(aa);
       return aa;
     } else if (cmd == "verbose") {
       SAFEtoken(aa,args);
       int b = atoi( aa.c_str());
       LIMP_VERBOSITY = b;
       return "";
     } else if (cmd == "wmatch") {
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);
       return wmatch(aa,bb)?"YES":"NO";
	} else if (cmd == "find") {
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);
       return itos(aa.find(bb));
	} else if (cmd == "word") {
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);
       return word(aa,atoi(bb.c_str()));
	} else if (cmd == "cut") {
       SAFEtoken(aa, args);
       SAFEtoken(bb, args);
       SAFEtoken(cc, args);
       return oktok(cut(aa,atoi(bb.c_str()),atoi(cc.c_str())));
     } else if (cmd == "limp-version") return LIMP_VERSION;
       else if (cmd == "fun") {
          /* define a user function */
          SAFEtoken(fname,args);
          string arglist  = NEXTtoken(args);
          string funcbody = NEXTtoken(args);

          if (arglist[0] != '(') {
              limp_error((string)"Parenthesized arg list must follow fun name in (fun "+ fname);
              RET_ERROR("SYNTAX");
          }
          
          arglist = losewhites(arglist.substr(1,arglist.length()-2));
               /* lose parentheses */

          limp_userfun_node * nax = new limp_userfun_node;

          string food;
          
          while ("" != (food = NEXTtoken(arglist))) {
              string s = EVALtoken(food);
              if ISret(s) { delete nax; return s; }
//              limp_debug((string)"Adding: ~G" + s);
              nax->vars.addstring(s);
          }

          nax->name = fname;
          nax->body = funcbody;

//          limp_debug((string)"Body: ~B" + funcbody);

          add_userfun ( limp_userfun_root, nax );          

          return "YES";
     } else if (cmd == "unfun") {
          /* FIXME */
          RET_ERROR("UNIMPLEMENTED");
     } else {
       ext_limp_token (*process)(stringqueue&)
          = limp_FUNC_ROOT->get(cmd);
       if (process) {
          // found user-function. Build args list:
          stringqueue mom; string food;
          while ("" != (food = NEXTtoken(args))) {
              string s = EVALtoken(food);
              CHECKret(s);
              mom.addstring(s);
          }
          ext_limp_token retz = process(mom);
          if (retz.type == LIMP_VALUE) {
               return retz.v;
          } else {
               limp_error((string)"Error in external function "+cmd);
               RET_ERROR("EXTFUNCERROR");
          }
       } else {
         limp_userfun_node * me = get_userfun(limp_userfun_root, cmd);
         if (me) {
             NEWSCOPE;
             /* introduce bindings... */

            /* FIXME: check interrupt for recursive runaways... */
            /* (probably would overflow stack. Max recursion counter? */

//          limp_debug((string)"Num args: ~r" + itos(me->vars.length()));

           for (uint xy=0;xy < me->vars.length();xy++) {
            // shadow:

            string aaa = EVALtoken(NEXTtoken(args));
            if (ISret(aaa)) {
                popscope(limp_ROOT);   /* hate that. */
                return aaa;
            }

//            limp_debug((string)"Binding ~g" + me->vars.stuff[xy] +
//               (string)"~e to ~g" + aaa);

            limp_ROOT->node->set(me->vars.stuff[xy],aaa);
           }

            /* run the body of the fun */

//            limp_debug((string)"Now evaluate : ~b" + me ->body);
            string retme = EVALtoken(me->body);

            popscope(limp_ROOT);
            CHECKret(retme); // after popping scope...
            return retme;
          } else {
              limp_error((string)"Unknown function in limp exp: "+ cmd);
              RET_ERROR("UNKNOWNFUNC");
          }
       }
	}
      }
    break;
    }
  case '`':

    return oktok(in.substr(1,in.length()-2));
    break;
  }
  return in; /* no specials, probably like evaltoken(14); */
}

string itos(int n) {
  char thing[512];
  sprintf(thing,"%d",n);
  return thing;
}

string nexttoken(string & in) { 

  in = losewhites(in); /* FIXME: losewhites is a little inefficient eh? */

  if (in == "") return oktok("");

  switch(in[0]) {
  case '%': {
     uint x;
     for (x=0;x<in.length();x++)
       if (whitespc(in[x]) || in[x] == ')' || in[x] == '(') break;
     string as = in.substr(0,x+1);
     in = ((x==in.length())?(string)"":(in.substr(x+1,in.length()-(x+1))));
     return as;
  }
  case '`': {
    for (uint x=1;x<in.length();x++) {
      if (in[x] == '`') {
	string as;
	as = in.substr(0,x+1);
	in = in.substr(x+1,in.length()-(x+1));
     return oktok(as);
      }
    }
    limp_error((string)"Unmatched ` in limp exp \"" +in+(string)"\".");
     RET_ERROR("SYNTAX");
    break; }
  case '\'': {
     /* shorthand for (quote (stuff)), so just get the next token after
        the quote and concatenate. */
     string ori = in; /* for error */
     in = in.substr(1,in.length()-1);
     string fnoop = NEXTtoken(in);
     if (fnoop =="") {
     limp_error ((string)"Poorly placed ' in expression \""+ ori+(string)"\".");
     RET_ERROR("SYNTAX");}
     return (string)"'" + fnoop;
  }
  case '(': {
    int pars=1;
    int inquotes=0;
    for (uint x=1;x<in.length();x++) {
      if (!inquotes) {
	if (in[x] == '`') inquotes=1;
	if (in[x] == '(') pars++;
	if (in[x] == ')') { 
	  pars--;
	  if (!pars) {
	    string as = in.substr(0,x+1);
	    in = in.substr(x+1,in.length()-(x));
         return oktok(as);
	  }
	}
      } else {
	if (in[x] == '`') inquotes=0;
      }
    }
    limp_error ((string)"Unmatched ()'s or ` in limp exp \""+ in+(string)"\".");
     RET_ERROR("SYNTAX");
    break; }
  default:
    for (uint x=1;x<in.length();x++) {
      if (whitespc(in[x])) {
	string as;
     as = string("`") + in.substr(0,x) + string("`");
//     limp_debug(as);
     in = in.substr(x,in.length()-(x));
     return oktok(as);
      }
    }
     string asp;
     asp = string("`") + in + string("`");
     in = "";
     return asp;
/*
    printf("BUILD IN THE THING WHERE YOU DON'T NEED `'s around everything!\n");
     exit(0);
     RET_ERROR;
*/
  }
printf("eo function?\n");
RET_ERROR("WTF");
return "";
}

string losewhites(string in) {
  uint x=0;
  while (whitespc(in[x]) && x<in.length()) { x++; }
  uint y=in.length()-1;
  while (whitespc(in[y]) && y>x) { y--; }
  return in.substr(x,(y-x)+1);
}

string word(string line,int foff) {
  line = ' ' + line + ' ';
  if (foff < 0) {
    /* count backwards: */
    int y = (-foff) -1, whites=1,endpos=-1;
    for (int x=line.length()-1;x--;) {
      if (whites) {

	if (!whitespc(line[x])) {
	  // reached nonwhite character.
	  if (!y) {
	    // this is our word.
	    endpos=x;
	    whites=0;
	  } else {
	    y--;
	    whites=0;
	  }
	}
      } else { // (!whites:)
	if (whitespc(line[x])) {
	  if (!y && endpos != -1) {
         return oktok(line.substr(x+1,endpos-x));
	  } else {
	    whites=1;
	  }
	}
      }
    }
    limp_error ((string)"There is no word "+ itos(foff)
     + (string)" in \"" + line + (string)"\"!");
     RET_ERROR("DOMAIN");
  } else { // foff is nonnegative
       /* count forwards: */
    int y = foff, whites=1,startpos=-1;
    for (uint x=0;x<line.length();x++) {
      if (whites) {
	if (!whitespc(line[x])) {
	  // reached nonwhite character.
	  if (!y) {
	    // this is our word.
	    startpos=x;
	    whites=0;
	  } else {
	    y--;
	    whites=0;
	  }
	}
      } else { // (!whites:)
	if (whitespc(line[x])) {
	  if (!y && (startpos != -1)) {
         return oktok(line.substr(startpos,x-startpos));
	  } else {
	    whites=1;
	  }
	}
      }
    }
    limp_error ((string)"There is no word "+ itos(foff)
     + (string)" in \"" + line + (string)"\"!");
     RET_ERROR("DOMAIN");
  }
}

string cut(string in, int a, int b) {

  int start;
  int len;

  if (a < 0) {
    start = in.length()+a;
    if (b) {
      len = b;
    } else {
      len = -b;
    }
  } else {
    start = a;
    len = b;
  }

  if ((uint)(start+len) > in.length()) {
    limp_error ("cut size [" + itos(a) + string(",") + itos(b)
               + string("] too "
         " long for \"") + in + "\".");
     RET_ERROR("DOMAIN");
  }
  return oktok(in.substr(start,len));
}

limp_node ::
limp_node ()
{l=r=NULL;}

string limp_node :: get (string test) {
  if (key==test) return value;
  else if (key<test) if (!r) return "`**NULL"; else return r->get(test);
  else               if (!l) return "`**NULL"; else return l->get(test);
}

void limp_node :: set (string &k, string &v) {
  if (key == k) { value=v; }
  else if (key < k) {
    if (r) r->set(k,v);
    else r = new limp_node(k,v,NULL,NULL);
  } else {
    if (l) l->set(k,v);
    else l = new limp_node(k,v,NULL,NULL);
  }
}

limp_node ::
limp_node (string a, string b, limp_node* le, limp_node* ri) {
  key = a;
  value=b;
  l=le;
  r=ri;
}

void limp_error(string s) {
     if (LIMP_VERBOSITY > -3)
       if(print_limp_error)(print_limp_error)(s);
}

void limp_debug(string s) {
     if (LIMP_VERBOSITY > -2)
       if(print_limp_debug)(print_limp_debug)(s);
}
void stringqueue ::
  addstring(string s) {
     if (num >= allocd) {
          string * temp = stuff;
          stuff = new string[allocd<<=1];
          if (!stuff) { printf("Out of memory!\n"); exit(-2); }
          for (uint x=0;x<num;x++)
             stuff[x] = temp[x];
          delete [] temp;
     }
     stuff[num++] = s;
}

ext_limp_token
   (*(limp_func_node :: get(string& k) ))(stringqueue& not_argument) {
     /* eaghh. */
  if (key==k) return func;
  else if (key<k) if (!r) return NULL; else return r->get(k);
  else            if (!l) return NULL; else return l->get(k);
}

void limp_func_node ::
  add_func(string & k, ext_limp_token(*fun)(stringqueue&)) {
  if (key == k) { func=fun; }
  else if (key < k) {
    if (r) r->add_func(k,fun);
    else r = new limp_func_node(k,fun);
  } else {
    if (l) l->add_func(k,fun);
    else l = new limp_func_node(k,fun);
  }
}

limp_node::~limp_node() {
     delete l;
     delete r;
}


void limp_regfuncstuffs(limp_funcstuff* smell) {
     int x=0;
     while (smell[x].name) {
          limp_regfunc(smell[x].name,smell[x].func);
     ++x;
     }
}

#if 0
/* DO NOT USE! copying of strinqueues seems to smash the stack */
stringqueue getpossiblefuncs(string s) {
     stringqueue n;
     limp_debug(s);
     possiblefuncs(s,limp_FUNC_ROOT,n);
//     if (n.length()) {
//          for (uint x=0;x<n.length();x++) limp_debug((string) "--->" +n.stuff[x]);
//     }
     return n;
}
#endif

void possiblefuncs(string s,limp_func_node*h,stringqueue&soap) {
     if (!h) return;
//     limp_debug((string)"Before: " + h->key);
     for (uint x=0;x<s.length();x++) {
          if (x >= h->key.length()) goto noway;
          if (s[x] != h->key[x]) goto noway;
     }
     soap.addstring(h->key);
//     limp_debug((string)"Added: " + h->key);
     noway:;
//     limp_debug((string)"Inspected key: " + h->key);

     if (h->l) possiblefuncs(s,h->l,soap);
     if (h->r) possiblefuncs(s,h->r,soap);
}

int limp_CALLTIMES;     
int limp_checkinterrupt() {
     return (limp_CALLTIMES++ > 10000 && limp_interrupt && limp_interrupt());
}

void limp_clearinterrupt() {
     limp_CALLTIMES=0;
}

int wmatch(string wc, string test) {
  // wc of form some*.*, etc.
  uint x;
  // walk up to the first * in wc
  for (x=0;x<wc.length();x++) {
    if (wc[x]=='*') break;
    else if (wc[x] != test[x]) return 0;
  }

  if (x>= wc.length()) return 1; // no *'s
  
  string frag;
  int bloc=x++; 
  // otherwise now look for fragments, repeat.
  for (;x<wc.length();x++) {
    if (wc[x] == '*') {
      if (frag == "") continue;
      if (((int)(bloc = test.find(frag,bloc))) == (int)(-1)) { /* FIXME: NPOS */
	return 0;
      } else { bloc += frag.length(); frag=""; }
    } else frag += wc[x];
  }
  // check remaining fragment
  if (frag == "") return 1;
  // walk through, must end the test string with frag
     if (frag.length() > test.length() ) return 0;

  test = test.substr(test.length()-frag.length(),frag.length());
  if (test.length() != frag.length()) return 0;
  for (x=0;x<frag.length();x++) 
     if (frag[x] != test[x]) return 0;
  return 1;
}

void add_userfun (limp_userfun_node *& head, limp_userfun_node * nu) {
     if (head) {
          if (head -> name == nu -> name) {
               /* FIXME? */
               /* Overwrite old function? */
               nu->l = head->l;
               nu->r = head->r;
               delete head; 
               head = nu;
          } else if (head -> name < nu -> name) {
                 add_userfun(head->r,nu);
          } else add_userfun(head->l,nu);
     } else head = nu;
}

limp_userfun_node * get_userfun (limp_userfun_node * head, string name) {
    while (head) {
          if (head -> name == name) return head;
          else if (head -> name <  name) head = head -> r;
          else head = head -> l;
    }
    return 0;
}
