
/* generate text ("homework") using a grammar description file
   
   Tom 7, 1997: licensed under the GPL

   Updated by Tom 7 in 2001 (how time flies...) with a %var=family%
   oneshot hack, %\n%, other cleanups, etc.

   In 2002, added the sorely needed #!familyname option to
   avoid repetition.

   Check out docs.txt for semi-accurate documentation. 

   $Id: homework.cc,v 1.13 2005/07/14 21:41:45 tom7 Exp $
*/

#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>

#define RECURSEMAX 1024

#define isvowel(c) (((c)=='e')||((c)=='a')||((c)=='i')||((c)=='o')||((c)=='u'))

#ifndef INCLUDEPATH
   #define INCLUDEPATH "/home/httpd/cgi-data/hw/"
#endif

string includepath = INCLUDEPATH;

/* win32 is crazy ?? */
#ifdef WIN32
   #define snprintf _snprintf
   #define random rand
   #define srandom srand
#endif

#define PREAMBLE "Content-type: text/html\n\n"
#define POSTSCRIPT "\n"

/* needed for some versions of g++, msvc++ */
using namespace std;

struct element {
  string value;
  int rep;
  int infinite;
  element * next;
  element(string val, int r, int i, element * n) : value(val), rep(r), 
						   infinite(i), next(n) {}
};

struct family {
  string name;
  element * contents;
  int length;
  family * next;
  int unique;
  int used;
  family(string nm, element * cts, int l, family * n, int un)
    : name(nm), contents(cts), length(l), next(n), unique(un), used(0) {}
};

/* XXX - more efficient symbol table might be appropriate */

struct variable {
  string name;
  string value;
  int valid; /* becomes 0 if unset */
  variable * next;
  variable(string n, string v, variable * nn)
    : name(n),
      value(v),
      next(nn),
      valid(1) {}
};

string typefile(char*);

// ********* GLOBAL VARIABLES *** //
static family * families = 0;
static int recursecount = 0;
static variable * sl = 0;
static element * currelement = 0;
// ****************************** //

void build(string,string);
string parse(string);
static int getline(string& out, FILE *&);
static int getrealline(string& out, FILE *&);
void addfamily(family *& head, family * newnode);
void addtofamily(family *& head, string name, element * newnode);
string percentcode(string inways);
string stoi(int i);

string stoi(int i) {
  char buf[64];
  sprintf(buf, "%d", i);
  return buf;
}

/* lookup a variable (%?hero%)
   1 if found, 0 if not
*/
int lookup(string name, string & result);
void setvar(string, string);

int main (int argc, char ** argv) {
  srandom(time(0) ^ getpid());
  string funame;
  int cgi = 1;

  for(int c = 1; c < argc; c++) {
    if (!strcmp(argv[c], "-n")) cgi = 0;
    else if (!strcmp(argv[c], "-d")) {
      if (c >= (argc-1)) {
	fprintf (stderr, "-d but no dir");
	exit(-1);
      }
      includepath = argv[++c];
    } else funame = argv[c];
  }

  if (funame == "") {
    fprintf(stderr,"%s inputfile\n", *argv);
    exit(-2);
  }

  if (cgi) printf("%s", PREAMBLE);

  build(funame, funame+": ");

  printf("%s%s\n", parse("%main%").c_str(), POSTSCRIPT);

  return 0;
}

string parse(string in) {
  /* read through the string, and substitute %familyname% where appropriate */
  recursecount++;
  if (recursecount > RECURSEMAX) { return "[[MAX RECURSION COUNT EXCEEDED]]"; }

  if (in == "") return "!![NULL]]!!";
  string output = "";
  int x;
  for (x=0;x<in.length();x++) {
    if (in[x]=='%') {
      /* leading %. */
      string inside ="";
      int n;
      for (n=x+1;n<in.length();n++) {
	if (in[n]=='%') {
	  output += percentcode(inside);
	  x = n;
	  goto I_use_gotos_with_pride;
	} else inside += in[n];
      }
      /* all the way to in.length()... bad. */
      printf("!![syntax error while parsing %%]]!!\n");
      return "[unclosed percent: "+in+"]";

    } else output += in[x];
  I_use_gotos_with_pride: ;
  }
  return output;
}

/* eval a conditional */
int conditional(string c) {
  if (c == "") {
    printf("!![syntax error: empty conditional]");
  }
  if (c[0] == '!') return !conditional(c.substr(1,c.length()-1));
  if (c == "rep") {
    return !!(currelement->rep - 1);
  } else {
    string dummy;
    return lookup(c,dummy);
  }
}

/* passed a string with percent signs stripped off.
   ("nonterminal" in the doc)
   Return its value.
*/
string percentcode(string inways) {
  if (inways=="") return "%";

  /* return the contents of a file */
  if (inways[0]=='*') {
    return typefile((char*)(inways.c_str()+1));
  }

  /* literal -- note quote is not closed */
  if (inways[0]=='\"') {
    return (char*)(inways.c_str()+1);
  }

  if (inways == "familystats") {
    /*
      struct family {
      string name;
      element * contents;
      int length;
      family * next;
      int unique;
      int used;
      };
    */
    string r = "<table>\n"
      "<tr><td>name</td><td>truelen</td><td>used</td><td>left</td><td></td><td>util</td></td>\n";

    for(family * tmp = families; tmp; tmp = tmp -> next) {
      int truelen = 0;
      for(element * cts = tmp->contents; cts; cts = cts -> next) truelen++;

      char res[512];
      snprintf(res, 510,
	       "<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%s</td><td>%f</td></tr>\n",
	       tmp->name.c_str(), truelen,
	       tmp->used, tmp->length, tmp->unique?"!":"", 
	       (float)tmp->used / truelen);
      r += res;
    }

    r +="</table>\n";
    return r;
  }

  /* escapes */
  if (inways[0]=='\\') {
    switch(inways[1]) {
    case 'n':
    case 'N':
      return "\n";
    }
    printf("unknown escape: \\%c\n", inways[1]);
    return "[[!unknown escape!]]";
  }

  /* look up a variable */
  if (inways[0]=='?') {
    string v = (char*)(inways.c_str() + 1);
    string s;
    if (lookup(v, s)) {
      return s;
    } else {
      return (string)"[[unset variable: " + v + (string)"]]";
    }
  }

  /* make upper-case */
  if (inways[0]=='^') {
    if (inways.length() > 1 && inways[1]=='^') {
      if (inways.length() > 2 && inways[2]=='^') {
	/* every letter */
	string r = percentcode((char*)(inways.c_str()+3));

	for(unsigned int i = 0; i < r.length(); i ++) {
	  if (r[i] >= 'a' && r[i] <= 'z') {
	    r[i] &= ~32;
	  }
	}

	return r;
      } else {
	/* all words */
	string r = percentcode((char*)(inways.c_str()+2));
	int cap = 1;
	for (unsigned int u=0; u < r.length(); u ++) {
	  if (cap && r[u] >= 'a' && r[u] <= 'z')
	    r[u] = r[u] & ~32;
	  if (r[u] == ' ' || r[u] == '\n' || r[u] == '\r' || r[u] == '\t'
	      || r[u] == '-' || r[u] == ';' || r[u] == '>') cap = 1;
	  else cap = 0;
	}
	return r;
      }
    } else {
      /* just initial letter */

      string r = percentcode((char*)(inways.c_str()+1));
      if (r.length() > 0 && r[0] >= 'a' && r[0] <= 'z') {
	r[0] = r[0] & ~32;
      }
      return r;
    }
  }

  /* make lower-case */
  if (inways[0]=='&') {
    string r = percentcode((char*)(inways.c_str()+1));
    if (r.length() > 0 && r[0] >= 'A' && r[0] <= 'Z') {
      r[0] = r[0] | 32;
    }
    return r;
  }

  /* prepend 'a ' or 'an ' depending on the first *letter* found (not in a <tag>) */
  if (inways[0]=='`') {
    string r = percentcode((char*)(inways.c_str()+1));
    for(int u=0; u < r.length(); u++) {
      if (u == '<') {
	/* eat until > or eos */
	while (u < r.length() && r[++u] != '>');
	continue;
      }
      if (u == ' ' || u == '\n' || u == '\r' || u == '\t') continue;
      if (isvowel(32|r[u])) return "an " + r;
      else return "a " + r;
    }
    return r;
  }

  /* increment a counter */
  if (inways[0]=='+') {
    string name = inways.c_str()+1;
    for(variable * ss = sl; ss; ss = ss -> next) {
      if (name == ss->name) {
	if (ss->valid) {
	  ss->value = stoi(atoi(ss->value.c_str()) + 1);
	} else {
	  ss->value = "1";
	}
	return "";
      }
    }
    /* add it */
    sl = new variable(name, "1", sl);
    return "";
  }

  /* unset variable */
  if (inways[0]=='-') {
    string name = inways.c_str()+1;
    for(variable * ss = sl; ss; ss = ss -> next) {
      if (name == ss->name) {
	ss->valid = 0;
	return "";
      }
    }
    /* return "" even if not found */
    return "";
  }

  /* conditional */
  if (inways[0]=='/') {
    /* read condition */
    string cond;
    for (int z = 1; z < inways.length(); z++) {
      if (inways[z] == '/') {
	if (conditional(cond)) {
	  /* condition true: continue */
	  return percentcode(inways.c_str() + z + 1);
	} else {
	  return "";
	}
      } else cond += inways[z];
    }
    return "[[syntax error -- unfinished conditional]]";
  }

  /* #(1,10) random syntax. */
  int method = 0;
  if (inways[0]=='#') {
    /* hex */
    if (inways[1]=='x') {
      method = 1;
      inways = (char*)(inways.c_str()+1);
    } else if (inways[1]=='a') {
      method = 2;
      inways = (char*)(inways.c_str()+1);
    }
    /* standard */
    if (inways[1]=='(') {
      int nn;
      for (nn=1;nn<inways.length();nn++) if (inways[nn]==',') {inways[nn]='\0'; break;}
      int upper=0;
      int lower = atoi((char*)(inways.c_str()+2));
      if (nn != inways.length()) {
	int zz;
	for (zz=nn+1;zz<inways.length();zz++) 
	  if (inways[zz] < '0' || inways[zz] > '9') { inways[zz]='\0';break;}
	upper = atoi((char*)(inways.c_str()+nn+1));
      }
      if (!upper) {
	// oy, they want 'lower' digits.
	char * set;
	int length;
	if (method==1) {
	  set = "0123456789";
	  length=9;
	} else {
	  set = "0123456789ABCDEF";
	  length=15;
	}
	string outward;
	for (;lower--;) outward += set[(int)((float(random())/RAND_MAX)*length)];
	return outward;
      } else {
	int num = lower+(int)((float(random())/RAND_MAX)*(upper-lower));
	char buffer[256];
	if (!method) sprintf (buffer,"%d",num);
	else if (method==1) sprintf(buffer,"%x",num);
	return buffer;
      }
    } else return "42";
  }

  /* doesn't begin with a control code. try %name=family% */
  string uu;
  int assignflag = 0;
  for (int z=0;z<inways.length();z++) {
    /*    printf("<br>inways='%s' inways[z]='%c' z=%d uu='%s'\n",
	  inways.c_str(), inways[z], z, uu.c_str()); */
    if (inways[z] == ':') { 
      assignflag = 1;
    } else if (inways[z] == '=') {
      if (assignflag) {
	/* :=, so always set and return "" */
	setvar(uu, percentcode((char*)(inways.c_str() + z + 1)));
	return "";
      } else {
	/* =, so set if unset, otherwise set and return contents */
	string s;
	if (lookup(uu, s)) {
	  return s;
	} else {
	  string rr = percentcode((char*)(inways.c_str() + z + 1)); 
	  setvar (uu,rr);
	  return rr;
	}
      }
    } else {
      uu += inways[z];
      assignflag = 0;
    }
  }
  
  /* standard %family% notation */

  family * tmp = families;
  while (tmp) {
    if (tmp->name == inways) {
      tmp->used ++;
      if (tmp->length <= 0) break;
      /* choose one at random: */

      int num = (int)((float(random())/float(RAND_MAX))*float(tmp->length));
      element * toop = tmp->contents;

      if (tmp->unique) {
	/*	printf("<br>num: %d\n", num); */
	/* find the nth unused/non-infinite entry */
	while (num || !(toop->infinite || !toop->rep)) {
	  /*
	    printf("<br>num: %d, toop: %p, toop->infinite: %d, toop->rep: %d, toop: %s\n",
	    num, toop, toop->infinite, toop->rep,
	    toop->value.c_str());
	  */
	  num -= (toop->infinite || !toop->rep);
	  toop = toop -> next;
	}
      } else {
	for (int z=0;z<num;z++) toop=toop->next;
      }

      toop->rep ++;

      /* update count if we removed a non-infinite entry */
      if (!toop->infinite && tmp->unique) tmp->length --;

      /* save old current element */
      element * tmp = currelement;
      currelement = toop;

      /* recurse */
      string m = parse(toop->value);

      /* restore current element */
      currelement = tmp;
      recursecount--;
      return m;

    } else tmp = tmp->next;
  }
  return "[["+inways+"]]";
}

void build(string fname, string errmsg) {
  fname = includepath + fname;
  FILE * infile = fopen(fname.c_str(),"r");
  if (!infile) { 
    printf("%sUnable to open file '%s'\n",
	   errmsg.c_str(), fname.c_str());
    exit(-1); 
  }
  // build loop
  int x;
  string thisline;
  string currentfam;
  int currunique = 0;
  while (1) {
    do if (!getline(thisline,infile)) {fclose(infile); return;}
    while (thisline == "");
    if (thisline[0]=='#') {
      const char * arg;
      /* if it is followed by !, then we need to set the unique flag */
      if (thisline[1]=='!') {
	currunique = 1;
	arg = thisline.c_str() + 2;
      } else {
	currunique = 0;
	arg = thisline.c_str() + 1;
      }
      for (x=1;x<thisline.length();x++) 
	if (thisline[x]==' ') { thisline[x]='\0'; break; }
      string argument = (char*)(thisline.c_str() + x+1);
      if (!strcmp(arg,"needs")) {
	build(argument,errmsg+argument+": ");
      } else if (!strcmp(arg,"include")) {
	printf("%s#include not yet supported (try #needs).\n",
	       errmsg.c_str());
      } else {
	/* new familyname. */
	if ((string)arg == "") { 
	  printf("%sempty familyname?\n", errmsg.c_str()); 
	  exit(-1);
	}
	currentfam = (string)arg;
	family * tmp = families;
	while (tmp) {
	  if (currentfam == tmp->name) { 
	    printf("%sduplicated familyname '%s'\n",
		   errmsg.c_str(),
		   currentfam.c_str());
	    exit(-1);
	  }
	  else tmp = tmp->next;
	}
	addfamily(families, new family(currentfam, 0, 0, 0, currunique));
      }
    } else {
      if (currentfam == "") {
	printf("%ssyntax error\n", errmsg.c_str());
	exit(-1);
      }
      if (currunique && thisline[0] == '!') {
	addtofamily(families,currentfam,
		    new element(thisline.c_str()+1, 0, 1, 0));
      } else {
	addtofamily(families,currentfam,
		    new element(thisline, 0, 0, 0));
      }
    }
  }
}

void addfamily(family *& head, family * newnode) {
  newnode->next = head; head=newnode;
}

void addtofamily(family *& head, string name, element * newnode) {
  /* this search should find the family immediately in normal use */
  
  family * tmp = head;
  while (tmp) {
    if (tmp->name == name) {
      newnode->next=tmp->contents; tmp->contents=newnode;
      tmp->length++;
      return;
    }
    tmp=tmp->next;
  }
  printf("familyname %s not found! \n", name.c_str());
  delete newnode;
}

/* XXX using += is pretty inefficient */
int getline(string& out, FILE * &file) {
  if (!getrealline(out, file)) return 0;
  if (out[out.length() - 1] == '\\') {
    string oo;
    if (!getline(oo, file)) return 0;
    out = out.substr(0, out.length() - 1) + oo;
    return 1;
  }
  return 1;
}

int getrealline(string& out, FILE * &file) {
  int c;
  out = "";
  for(;;) {
    /* this doesn't return the last line if there is no \n on it */
    do if ((c=fgetc(file))==EOF) return 0; while (c == '\r');
    
    if (c == '\n') return 1;

    out += (char)c;
  }
}

/* XXX this should read in chunks! */
string typefile(char* name) {
  FILE * infile;
  int a;
  string out;
  if ((infile = fopen(name,"rb"))) {
    while ((a = fgetc(infile)) != EOF)
      out += (char)a;
    fclose(infile);
  }
  else out = "I can't open " + (string)name + "... =(";
  return out;
}

void setvar(string name, string val) {
  for(variable * ss = sl; ss; ss = ss -> next) {
    if (name == ss->name) {
      ss->valid = 1;
      ss->value = val;
      return ;
    }
  }

  /* not found, make a new entry */
  sl = new variable(name, val, sl);
  
}

int lookup(string name, string & res) {

  for(variable * ss = sl; ss; ss = ss -> next) {
    if (name == ss->name) {
      if (ss->valid) {
	res = ss->value;
	return 1; 
      } else return 0;
    }
  }
  return 0;

}

