/* limp is distributed under the Gnu Public License. See
    http://gnu.org/copyleft/gpl.html
   for licensing details. */

#ifndef LUDUS_LIMP_H
#define LUDUS_LIMP_H
#define uint unsigned int
#include <setjmp.h>

#define whitespc(c) ((c)==' '||(c)=='\t'||(c)=='\n'||(c)=='\r')

#define LIMP_ERROR_TOKEN ((ext_limp_token){(string)"ERROR",LIMP_ERROR})

enum limp_type { LIMP_NOTHING, LIMP_VALUE, LIMP_ERROR, };

struct ext_limp_token {
  string v;
  limp_type type;
};

extern void (*print_limp_error)(string);
extern void (*print_limp_debug)(string);
extern int  (*limp_interrupt)();

int  limp_checkinterrupt();
void limp_clearinterrupt();

struct limp_node { /* kinda like lymph node? */
  string key, value;
  limp_node * l, * r;
  string get(string);
  limp_node();
  limp_node(string,string,limp_node*,limp_node*);
  void set(string&,string&);
 ~limp_node();
};

struct limp_node_stack {
  limp_node * node;
  limp_node_stack * next;
  limp_node_stack(limp_node_stack*,limp_node*);
  string get(string);
//  void   set(string&,string&);
};

class stringqueue {
public:
  string * stuff;
  void addstring(string s); 
  stringqueue() {
     stuff = new string[1];
     allocd = 1;
     num = 0;
  }
 ~stringqueue() {
//     print_limp_debug("~rDestroyed a stringqueue.");
     if (stuff) delete [] stuff;
  }
  uint length() { return num; }
private:  
  stringqueue(const stringqueue &); /* avoid copies */
  uint num;
  uint allocd;
};


struct limp_func_node {
  string key;
  limp_func_node *l, *r;
  ext_limp_token (*func)(stringqueue&);
  limp_func_node() { l = r = NULL; func = NULL; }
  limp_func_node(string x,ext_limp_token(*fnc)(stringqueue&)) {
          l = r = NULL; func = fnc; key = x;}
  void add_func(string&,ext_limp_token(*fun)(stringqueue&));
  ext_limp_token
   (*(get(string&) ))(stringqueue&); /* ooh. Check out my mad declaration
                                                       skills yo. */
};

struct limp_userfun_node {
  string name;
  stringqueue vars;
  string body;
  limp_userfun_node *l , * r;

  limp_userfun_node() { l = r = 0; } /* HERE */
};

struct limp_funcstuff {
  char * name; // or NULL for end
  ext_limp_token (*func)(stringqueue&);
};

void limp_regfuncstuffs(limp_funcstuff*);

void limp_setval(string,string),
     limp_init(),
     limp_error(string),
     limp_debug(string);

int istrue(string exp);

string eval_limp(string),
       losewhites(string in),
       itos(int);

string limp_getval(string key);

//limp_token
//       oktok(string),
string
       nexttoken(string & in),
       evaltoken(string in),
       cut(string,int,int),
       word    (string,int);

int wmatch(string wc, string test);

void limp_regfunc(string name, ext_limp_token (*f)(stringqueue&));

void possiblefuncs(string s,limp_func_node*h,stringqueue&soap);
stringqueue getpossiblefuncs(string s);

void limp_global_set(string k,string v);

void pushscope(limp_node_stack * & rut, limp_node * sfoot);

extern limp_node_stack * limp_ROOT;
extern limp_func_node  * limp_FUNC_ROOT;

/* userfun stuff: */
void add_userfun (limp_userfun_node *& head, limp_userfun_node * nu);
limp_userfun_node * get_userfun (limp_userfun_node * head, string name);
/* ************** */

extern int LIMP_VERBOSITY;

#define VERB if (LIMP_VERBOSITY >= 0)

#endif
