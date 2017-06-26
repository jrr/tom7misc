#include <string>
//#include <fstream.h>
#include <stdio.h>
#include <iostream.h>
//#include <iomanip.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
//#include <values.h>

#define RECURSEMAX 1024

#define MAX_INT 0x7FFFFFFF

#define isvowel(c) (((c)=='e')||((c)=='a')||((c)=='i')||((c)=='o')||((c)=='u'))

#define INCLUDEPATH "/usr/home/snoot/cgi-data/hw/"

#define PREAMBLE "Content-type: text/html\n\n"
struct element {
       string value;
       int count;
       element * next;
};

struct family {
       string name;
       element * contents;
       int length;
//     string last;
       family * next;
};

// ********* GLOBAL VARIABLES *** //
family * families = NULL;
int recursecount = 0;
// ****************************** //

void build(string,string);
string parse(string);
int getline(string& out, FILE *&);
void addfamily(family *& head, family * newnode);
void addtofamily(family *& head, string name, element * newnode);
string percentcode(string inways);

int main (int argc, char ** argv) {
    srandom(time(NULL));
    string funame;
    if (argc < 2) {
        cerr << argv[0] << " inputfile\n";
        exit(0);
    } else {
        funame = argv[1];
    }

    build(funame, funame+": ");
    cout << PREAMBLE << parse("%main%") << endl;
}

string parse(string in) {
    // read through the string, and substitute %familyname% where appropriate
    recursecount++;
    if (recursecount > RECURSEMAX) { return "[[MAX RECURSION COUNT EXCEEDED]]"; }
//cout << "parse("<<in<<")\n";
    if (in == "") return "!![NULL]]!!";
    string output = "";
    int x;
    for (x=0;x<in.length();x++) {
        if (in[x]=='%') {
//           if (x==(in.length()-1)) { }
           // leading %.
           //if (in[x+1]=='%') { output += '%'; x++; continue; }
           string inside ="";
           int n;
           for (n=x+1;n<in.length();n++) {
               if (in[n]=='%') {
                  output += percentcode(inside);
                  x = n;
                  goto I_use_gotos_with_pride;
               } else inside += in[n];
           }
           // all the way to in.length()... bad.
           cout << "!![syntax error while parsing %]]!!\n"; return "[unclosed percent: "+in+"]";

        } else output += in[x];
I_use_gotos_with_pride: ;
    }
    return output;
}

string percentcode(string inways) {
    if (inways=="") return "%";
//    cout << "percentcode("<<inways<<").\n";
    int method = 0;
    if (inways[0]=='#') {
       if (inways[1]=='x') { // hex
          method = 1;
          inways = (char*)(inways.c_str()+1);
       } else if (inways[1]=='a') {
          method = 2;
          inways = (char*)(inways.c_str()+1);
       }
       if (inways[1]=='(') { // standard
          int nn;
          for (nn=1;nn<inways.length();nn++) if (inways[nn]==',') {inways[nn]='\0'; break;}
          int upper=0;
          int lower = atoi((char*)(inways.c_str()+2));
          if (nn != inways.length()) {
          int zz;
          for (zz=nn+1;zz<inways.length();zz++) if (inways[zz] < '0' || inways[zz] > '9') { inways[zz]='\0';break;}
          upper = atoi((char*)(inways.c_str()+nn+1));
          }
//          cout << "(((I randomed: " << lower << " to " << upper << ".)))";
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
             for (;lower--;) outward += set[(int)((float(rand())/RAND_MAX)*length)];
             return outward;
          } else {
          int num = lower+(int)((float(rand())/RAND_MAX)*(upper-lower));
          char buffer[256];
          if (!method) sprintf (buffer,"%d",num);
          else if (method==1) sprintf(buffer,"%x",num);
          return buffer;
          }
          } else return "42";
       }
    int capitalize=0;
    int lowercase=0;
    int an=0;
    if (inways[0]=='^') { capitalize++; inways = (char *)(inways.c_str()+1);}
    if (inways[0]=='&') { lowercase++;  inways = (char *)(inways.c_str()+1);}
    if (inways[0]=='`') { an++;         inways = (char *)(inways.c_str()+1);}
    family * tmp = families;
    while (tmp) {
          if (tmp->name == inways) {
             if (!tmp->length) break;
             // choose one at random:
             //cout << "<b>There are "<< tmp->length <<" and I chose ";
             int num = (int)((float(random())/float(MAXINT))*float(tmp->length));
             //cout << num << "</b>\n";
             element * toop = tmp->contents;
             if (num) for (int z=0;z<num;z++) toop=toop->next;
             string m = parse(toop->value); recursecount--;
             if (an) {if (isvowel(32|m[0])) m = "an "+m;
                  else m = "a "+m;
                  }
             if (isalpha(m[0])) m[0] &= ~(capitalize<<5);
             if (lowercase && isalpha(m[0]) ) m[0] |= 32;
             return m;
          } else tmp = tmp->next;
    }
    return "[["+inways+"]]";
//    return "I dunno.";
}

void build(string fname, string errmsg) {
//    cout << "Building database from [" << fname << "]...\n";
//    ifstream infile(fname.c_str());
    fname = INCLUDEPATH + fname;
    FILE * infile = fopen(fname.c_str(),"r");
    if (!infile) { cout << errmsg<< "Unable to open file " <<fname<<endl;exit(-1); }
    // build loop
    int x;
    string thisline;
    string currentfam;
    while (1) {
        do if (!getline(thisline,infile)) {fclose(infile); return;}
                      while (thisline == "");
//        cout << "Build with line: "<<thisline<<endl;
        if (thisline[0]=='#') {
//           thisline += ' ';
         char * arg = (char*)(thisline.c_str()+1);
         for (x=1;x<thisline.length();x++) if (thisline[x]==' ') { thisline[x]='\0'; break; }
         string argument = (char*)(thisline.c_str() + x+1);
            if (!strcmp(arg,"needs")) {
                build(argument,errmsg+argument+": ");
             } else if (!strcmp(arg,"include")) {
                cout <<errmsg<< "include not supported yet.\n";

             } else {
                // new familyname.
                if ((string)arg == "") { cout <<errmsg<< "empty familyname?\n"; exit(-1); }
                currentfam = (string)arg;
                family * tmp = families;
                while (tmp) {
                       if (currentfam == tmp->name) { cout << errmsg<< "duplicated familyname "<< currentfam<<"\n"; exit(-1); }
                       else tmp = tmp->next;
                       }
                addfamily(families,new family((family){currentfam,NULL,0,0}));
//                currentfam = (string)arg;
             }
        } else {
               if (currentfam == "") { cout << errmsg<< "syntax error\n"; exit(-1); }
               addtofamily(families,currentfam,new element((element){thisline,0,NULL}));
        }
    }
}

void addfamily(family *& head, family * newnode) {
//cout << "add family: " << newnode->name << endl;
     newnode->next = head; head=newnode;
}

void addtofamily(family *& head, string name, element * newnode) {
// could cache this search, since these are all added in order
// ...fortunately the families are also added at the head, so
// they should be the first one anyway!
//     cout << "adding " << newnode->value << " to " << name << ".\n";
     family * tmp = head;
     while (tmp) {
     if (tmp->name == name) {
          newnode->next=tmp->contents; tmp->contents=newnode;
          tmp->length++;
          return;
     }
     tmp=tmp->next;
     }
     cout <<"familyname "<<name<<" not found! \n";
     delete newnode;
}
/*
void recurseaddtofamily(element *& head, element * newnode) {
     if (!head) newnode = head; else recurseaddtofamily(head->next);
}
*/

int getline(string& out, FILE * &file) {
    int c;
    out = "";
    for(;;) {
    if ((c=fgetc(file))==EOF) return 0; // last line not counted if missing \n
  //  cout <<"["<< (char)c <<"]" << flush;
    if (c!='\r') {
       if (c=='\n') return 1;
       out += (char)c;
    }
    }
}
