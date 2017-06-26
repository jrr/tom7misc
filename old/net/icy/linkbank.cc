
#include "linkbank.h"
#include "icy.h"
#include "aux.h"
#include <fstream.h>
#include <stdio.h>

/*
  #define STARTWORDS 5
  #define USEDWORDS  5
*/
linkbank::linkbank() {
  words = new wordlink[LB_STARTWORDS];
  if (!words) {
    big_error("Out of memory in linkbank::linkbank()");
  }
  words[0].word = "<BEGIN>";
  words[1].word = "<END>";
  used=2;
  index = 0;
  size=LB_STARTWORDS;
  srandom(time(NULL));
}

struct wuhh {
  string word;
  int freq;
  wuhh () {word="(nil)"; freq=-1; }
};

string linkbank::info (int length) {
  wuhh starts[length];
  wuhh overall[length];
  int  startnum=0,
    mega[used]; /* yay, g++! */
  for (int x=0;x<used;x++) mega[x]=0;

  for (int x=0;x<used;x++) {
    wnode * temp = words[x].links;
    while (temp) {

      if (x==0) {
	// Starting word.  Insert-in-order for startword list.
	int minimum = 0xFFFFFF;
	int minidx = -1;
	startnum += temp->metric;
	
	for(int ib=0;ib<length;ib++) {
	  if (starts[ib].freq < temp->metric 
	      && starts[ib].freq < minimum) {
	    minidx = ib;
	    minimum = starts[ib].freq;
	  }
	}
	
	if (minidx != -1) {
	  starts[minidx].word = words[temp->idx].word;
	  starts[minidx].freq = temp->metric;
	}
      }

      // increment the destination:
      ++      mega[temp->idx];
      

      temp = temp -> next;
    }
  }
  // do the same thing to find the 5 biggest words:
  
  mega[0] = mega[1] = 0; /* lame! */
  for (int x=0;x<used;x++) {
    int ominimum = 0xFFFFFF;
    int ominidx = -1;
    
    for(int i=0;i<length;i++) {
      if (overall[i].freq < mega[x] 
	  && overall[i].freq < ominimum) {
	ominidx = i;
	ominimum = mega[x];
      }
    }

    if (ominidx != -1) {
      overall[ominidx].word = words[x].word;
      overall[ominidx].freq = mega[x];
    }
  }
  
  char infox[4096];
  char smally[1024];
  sprintf(infox, "icy version " VERSION ": %d total words in "
	  "database. Most common starting words: ",used);
  for (int x=0;x<length;x++) {
    sprintf(smally,  "\"%s\" (%.1f%%), ", 
	    starts[x].word.c_str(), starts[x].freq * 100./ startnum);
    strcat(infox, smally);
  }
  strcat(infox, "Most common words overall: ");

  for (int x=0;x<length;x++) {
    sprintf(smally, "\"%s\" (%d), ",
	    overall[x].word.c_str(), overall[x].freq /* 100./ used*/
	    );
    strcat(infox,smally);
  }

  return
    infox;
}


int linkbank::savetofile(const char * filename) {
  ofstream outie(filename);
  if (!outie) {
    cout << "!!!! CAN'T OPEN " << filename << " for writing!\n";
    return 0;
  }
  for (int x=0;x<used;x++) {
    outie << x << ' ' << words[x].word << endl;
    wnode * temp = words[x].links;
    while (temp) {
      outie << "   " << (int) temp->idx << ' '
	    << (int) temp->metric << ' ' << (int) temp->how << endl;
      temp = temp -> next;
    }
    outie << "   0\n";
  }
  outie.close();
  cout << "! Wrote to " << filename << iendl;
  changed=0;
  return 1;
}

linkbank::linkbank( const char * filename) {
  words = new wordlink[LB_STARTWORDS];
  if (!words) {
    big_error("Out of memory in linkbank::linkbank()");
  }
  words[0].word = "<BEGIN>";
  words[1].word = "<END>";
  used=2;
  size=LB_STARTWORDS;
  srandom(time(NULL));
  index = 0;
  /*
    big_error("linkbank(filename) Not implemented");
  */
  ifstream innie(filename);
  if (!innie) {
    big_error("Can't open file!");
  }

  int num; string wrrd;

  /* This is incredibly stupid, since it basically builds the
     entire list as if it were analyzing text. */
  // int a;
  while (innie >> num >> wrrd) {

    setwordidx(wrrd,num);
    
    if (used <= num) used = num+1;

    int where, metric, how;
    
    do {
      innie >> where;
      
      if (where) {
	innie >> metric >> how;
	addlink(num,where,(linkway)how,metric);
      }
    } while (where);
  }
  innie.close();
  cout << "! Done reading " << filename << iendl;
}

string linkbank::construct() {
  // walk the linkbank and construct a phrase.
  
  int iters = MAXITER;
    
  int here = 0; // <BEGIN>
  int next = 1; // just in case no links at 0
  linkway how = HOW_NOTHING;
  string phrase;

  while (iters--) {
    if (here==1) {
      // <END>
      return phrase;
    }

    wnode * fee = words[here].links;
    int count=0;
    while (fee) {
      float sup = (float(random())/RAND_MAX);
      if (sup < float(fee->metric)/ (count+=fee->metric)) {
	next = fee->idx;
	how  = fee->how;
      }
      fee = fee->next;
    }

    switch (how) {
    case HOW_AMPERSAND: phrase += " & "; break;
    case HOW_SPACE:     phrase += ' '; break;
    case HOW_DOT:       phrase += '.'; break;
    case HOW_DOTSPACE:  phrase += ". "; break;
    case HOW_COMMA:     phrase += ", "; break;
    case HOW_BANG:      phrase += "! "; break;
    case HOW_SEMI:      phrase += "; "; break;
    case HOW_COLON:     phrase += ": "; break;
    case HOW_DOTDOTDOT: phrase += "... "; break;
    case HOW_QUESTION:  phrase += "? "; break;
    case HOW_NOTHING:
    default:;
    }
    if (next >= 2) phrase += words[next].word;
    here = next;
  }
  return "(gave up)";
}

void linkbank::addphrase(string aa) {
  changed=1;
  add_helper(aa,0,HOW_NOTHING);
}

void linkbank::addlink(int whence, int to, linkway how, int met) {
  if (!met) {
  cout << words[whence].word /*<< '('
       << (int) whence << ")" */ << " ->[" << (int) how
       << "]->" <<  words[to].word << '(' << (int) to << ')'; //<< iendl;
  }
  // make link:
  wnode * temp = words[whence].links;
  while (temp) {
    if (temp->idx == to && temp->how == how) {
      // already have this link. increase metric.
      (temp->metric)++;
      cout << " {" << temp->metric << '}' << iendl;
      return;
    }
    temp = temp -> next;
  }
  words[whence].links = new 
    wnode((wnode){to,(met)?:1,how,words[whence].links});
  //  if (!met) cout << " {new}" << iendl;
  if (!met) cout << iendl;
}

void linkbank::add_helper(string a, int whence, linkway whow) {
  // get first token, ender:
  for (;;){
  linkway thow;
  string wrd;
  int here;
  uint x;
try_it_again:
  a = losewhites(a);
  
  if (a == "") {
    // done.
    addlink(whence,1,whow,0); // link to <END>
    return;
  }

  for (x=0;x<a.length();x++) {
      // ok, advance and decide what it is.
      switch (a[x]) { 
      case '!':
	thow = HOW_BANG; x++; goto gotcha;
      case ' ':
	thow = HOW_SPACE; x++; goto gotcha;
      case '?': x++; thow=HOW_QUESTION; goto gotcha;
      case ',':
	thow = HOW_COMMA; x++;
	goto gotcha;
	break;
      case '.':
	// period-space or period or ...?
	if (x==(a.length()-1)) { ++x; thow = HOW_DOT; goto gotcha;}
	if (a[x+1] == ' ') { 
	  x+=2;
	  thow = HOW_DOTSPACE; 
	  goto gotcha;
	}
	if (a[x+1] == '.') {
	  while (x<a.length() && a[x++]=='.');
	  if (a[x-1] != ' ') x--;
	  thow = HOW_DOTDOTDOT;
	  goto gotcha;
	} ++ x;
	thow = HOW_DOT; goto gotcha;
	break;
      case '&':x ++; thow = HOW_AMPERSAND; goto gotcha;
      case ':':x ++;
	thow = HOW_COLON; goto gotcha;
      case ';':++            x;
	thow = HOW_SEMI; goto gotcha;
      case '\"':break;
      default: wrd+=a[x];
      }
  }
  thow = HOW_NOTHING; // got to eol

 gotcha:

  //  make the link from the last place:
  a = a.substr(x,a.length()-x);

  if (wrd == "") {
    // this is like "bob . . . . . . ". cycle through this crap;
    // reduce to valid tokens.
    goto try_it_again;
  }
  
  here = wordidx(wrd);

  addlink(whence,here,whow,0);

  // call tail-recursively:
  whence = here;
  whow = thow;
  
  }  
}

void linkbank :: setwordidx(string in, int place) {
  while (!(place < size)) {
    cout << " doubling words[] to " << (size<<1) << iendl;
    // ok, allocate more and copy (ugh):
    wordlink * temp = new wordlink[size<<1];
    if (!temp) {
      big_error("Out of memory doubling wordbank.\n");
    }
    for (int u=0;u<used;u++) {
      temp[u].word  = words[u].word;
      temp[u].links = words[u].links;
    }
    delete [] words;
    words = temp;
    size<<=1;
  }
  
  words[place].word = in;

  /* insert in itable */

  if (!index) index = new itable(place);
  else index->insert(place,this);
}

int linkbank :: wordidx(string in) {

  // make it better: ***************FIXME ************
  // find in itable: 
#if 1
  itable * t = index;

  while (t) {
    int r = strcmp(in.c_str(), words[t->idx].word.c_str());
    if (!r) return t->idx;
    else if (r<0) t = t->l; 
    else t = t->r;
  }

#else
  for (int u=0 /*2*/;u<used;u++) {
    if (in == words[u].word) return u;
  }
#endif

  // not here. Add, continue.

  setwordidx(in,used);
  return used++;
}
