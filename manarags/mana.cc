//--------------------------------------------------------------------------
//                                              Manarags II
//  anagram generator by Tom 7
//--------------------------------------------------------------------------

#include <string>
#include <stdlib.h>
#include <fstream.h>
#include <iostream.h>

#define MAXWORDLEN 32
#define STATMOD 1000

using namespace std;

struct element {
       string word;
       element * next;
};

int    printlist(element*);
void   destroy  (element**);
void   buildlist(string,element**&);
string subtract (string test, string in);
void   anagram  (string, string, element**);
int    filter   (int testlen, const char * test,string in);

element * forced = NULL;

string input;
int RECURSION,STATUS;
int QUIET=0, MINLEN=0, BIGGEST=0;

string lcase(string in);
string lcase(string in) {
  for(unsigned int i= 0; i < in.length(); i++) {
    if (in[i] >= 'A' &&
	in[i] <= 'Z') in[i] = in[i] | 32;
  }
  return in;
}

main (int argc,char**argv) {
string infile = "wordlist.asc";
     if (!argv[1]) {
        cerr << "Manarags 2 by Tom Murphy 7\n"
                "http://www.andrew.cmu.edu/~twm/mana/\n\n"
                "mana [-q][-i wordfile][-m minlen][-f forced]\n"
	        "       [-f forced][...][-b biggestwords] phrasetoanagram\n\n"
                "recommended that you redirect to a file:\n\n"
                "mana lambdacalculus > lambda.txt\n";
        exit(-1);
     }
     for (int x=1;x<argc;x++) {
        if ('-' == argv[x][0]) {
           switch (32 | argv[x][1]) {
                  case 'q': QUIET++; break;
                  case 'm':
                     MINLEN = atoi(argv[++x])-1; break;
                  case 'f': {
                     forced = new element((element){(string)lcase(argv[++x]),forced});
                     break; }
                  case 'b':
                     BIGGEST = atoi(argv[++x]); break;
                  case 'i':
                     infile = argv[++x]; break;
                  default: cerr << "What switch is this? -> -"<<argv[x][1]<<endl;
           }
        } else { input = lcase(argv[x]); break; }

     }
     if (input == "") { cerr << "What, no phrase?\n"; exit(0); }

     if (element * fd = forced) {
       cerr << "Forced: ";
       while (fd) {
             cerr << fd->word;
             input = subtract(fd->word,input);
             fd = fd->next;
       }
       cerr << endl;
     }


     element ** wordlist = (element**) calloc(MAXWORDLEN,sizeof(element*));

     buildlist(infile, wordlist);
     if (BIGGEST) {
        int n = MAXWORDLEN;
        while (--n>=0 && printlist(wordlist[n]));
     } else {
        if (!QUIET) cerr << "Anagramming...\n";
        RECURSION=STATUS=0;
        anagram("",input,wordlist);
     }
     destroy(wordlist);
     free(wordlist);
}

void anagram(string done, string left, element**words) {
  if (!left.length()) {
     element * fd = forced;
     while (fd) {
           cout << fd->word << " ";
           fd = fd->next;
     }
     cout << done << endl;
     if (!QUIET) { if (!(STATUS % STATMOD)) cerr << STATUS << ": " << done << endl;
     STATUS++;   }
     return;
  }

  int n = left.length()-1;
  if (n > MAXWORDLEN) n=MAXWORDLEN;

  for (;n>=MINLEN;--n) {
      if (!RECURSION && !QUIET) cerr << "[" << n+1 << "]\n";
      element * h = words[n];
      while (h) {
        if (filter(h->word.length(),h->word.c_str(),left)) {
           RECURSION++;
           anagram(done + h->word + " ",subtract(h->word,left),words);
           RECURSION--;
        }
      h = h->next;
      }
  }
}

void destroy(element** ha) {
  element * t, * f;
  for (int n=0;n<MAXWORDLEN;n++) {
     f = ha[n];
     while (f) {
       f=(t=f)->next;
       delete t;
     }
  }
}

int printlist(element*h) {
  while (h) {
    cout << h->word << endl;
    h = h->next;
    if (!--BIGGEST) return 0;
  }
  return 1;
}

void buildlist(string fname,element**&wlist) {
   ifstream infile(fname.c_str());
   string newword;
   while (infile >> newword)
      if (newword.length() >= MINLEN && newword.length() <= input.length() && newword.length() <= MAXWORDLEN && filter(newword.length(),newword.c_str(),input))
         wlist[newword.length()-1] = new element((element){newword,wlist[newword.length()-1]});
   infile.close();
}

string subtract(string test, string in) {
  int u,v;
  string out;
     for (u=0;u<test.length();u++) {
         for (v=0;v<in.length();v++)
            if (test[u] == in[v]) { in[v] = '!'; goto okay;}
         cout << "ERROR!\n";
         exit(-1);
     okay:;
     }
     for (u=0;u<in.length();u++)
         if (in[u] != '!') out += in[u];
   return out;
}

int filter(int testlen, const char * test,string in) {
   int u,v;
   for (u=0;u<testlen;u++) {
       for (v=0;v<in.length();v++)
          if (test[u] == in[v]) { in[v] = '!'; goto okay;}
       return 0;
   okay:;
   }
   return 1;
}

