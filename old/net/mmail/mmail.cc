
#include <stdio.h>
#include <iostream.h>
#include <string>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#define MAILCMD "sendmail"

#define HEADER "header.txt"
#define FOOTER "footer.txt"

void addfile(FILE * f, const char * s);
string fixupname(string i);

int main (int argc, char ** argv) { 

  if (argc < 3) { 
    printf("%s \"subject\" \"from@email\"\n"
	   "(run as root if email is not you)\n",*argv); 
    exit(-1);
  }

  string subject = argv[1];
  string from    = argv[2];

  int count = 0;
  string name,email;
  while (cin >> name >> email) {

    if (strchr(name.c_str(),'@') ||
	!strchr(email.c_str(),'@')) {
      printf("ERROR: name/e-mail pair appears to be mismatched "
	     "(name: %s, email: %s)\n",
	     name.c_str(),
	     email.c_str());
      exit(-1);
    }

    char fname[256];
    sprintf(fname,"temp.%d.%d.%s", getpid(), count, name.c_str());
    FILE * outie = fopen(fname,"w");

    fprintf(outie, "Subject: %s\n", subject.c_str());
    fprintf(outie, "To: %s\n", email.c_str());
    fprintf(outie, "X-Mailer: mmail 1.0\n\n");

    addfile(outie, HEADER);
	 
    fprintf(outie,"Dear %s,\n\n",fixupname(name).c_str());

    addfile(outie, FOOTER);
    fclose(outie);

    char syscmd[256];
    sprintf(syscmd,MAILCMD " \"-f%s\" \"%s\" < %s", 
	    from.c_str(), email.c_str(), fname);
    printf("%s # (%d to %s)\n",syscmd,++count, email.c_str());
    system(syscmd);
    printf("... done\n");
    remove(fname);
  }
  printf("Done! I sent %d mail%s.\n", count, count==1?"":"s");
}

void addfile(FILE * f, const char * s) {
  FILE * addme = fopen(s,"r");
  if (!addme) {
    printf("ARG! Can't open [%s]...\n", s);
    exit(1);
  } else {
    int a;
    while (EOF != (a = fgetc(addme))) { fputc(a,f); }
    fclose(addme);
  }

}

string fixupname(string i) {

  for (unsigned int x = 0; x < i.length() ; x ++ ) 
    if (i[x] == '_') i[x] = ' ';

  return i;

}
