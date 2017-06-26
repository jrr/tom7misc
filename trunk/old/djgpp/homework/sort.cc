#include <string>
//#include <fstream.h>
#include <stdio.h>
#include <iostream.h>
//#include <iomanip.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <values.h>

#define RECURSEMAX 1024

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

void recurseaddtofamily(element *& head, element * newnode);

// ********* GLOBAL VARIABLES *** //
family * families = NULL;
int recursecount = 0;
// ****************************** //

void printthem(family *);

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
        cout << "Input file: "; cin >> funame;
    } else {
        funame = argv[1];
    }

    build(funame, funame+": ");

    printthem(families);
}

void printthem(family * me) {
     if (me) {
        printthem(me->next);
        cout << endl << "#" << me->name << endl;
        element * h = me->contents;
        while (h) {
           cout << h->value << endl;
           h=h->next;
        }
     }
}

void build(string fname, string errmsg) {
//    cout << "Building database from [" << fname << "]...\n";
//    ifstream infile(fname.c_str());
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
               cout << "#needs " << argument << endl;
//               build(argument,errmsg+argument+": ");
             } else if (!strcmp(arg,"include")) {
               cout << "#include " << argument << endl;
//                cout <<errmsg<< "include not supported yet.\n";

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
          recurseaddtofamily(tmp->contents,newnode);
          return;
     }
     tmp=tmp->next;
     }
     cout <<"familyname "<<name<<" not found! \n";
     delete newnode;
}

void recurseaddtofamily(element *& head, element * newnode) {
    if (!head || newnode->value < head->value)  {
        newnode->next = head;
        head = newnode; return; }

    if (head->next) {
      if (head->next->value >= newnode->value) {
         newnode->next = head->next;
         head->next = newnode;
      } else recurseaddtofamily(head->next, newnode);
    } else recurseaddtofamily(head->next, newnode);
}

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
