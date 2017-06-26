
#include <string>
#include "util.h"
#include "dirent.h"

using namespace std;

bool nameok(string);
string newname(string);

int main (int argc, char ** argv) {

  FILE * out = fopen("index.html", "w");

  fprintf(out,
	  "<html><head><title>Mechanical Life Vein</title></head>\n"
	  "<body bgcolor=\"#FFFFFF\">\n"
	  "<div style=\"font : 30px Verdana Bold\">\n"
	  "The Mechanical Life Vein.\n");

  for(char subdir = 'a'; subdir <= 'z'; subdir++) {
    char dir[2] = " ";
    dir[0] = subdir;
    DIR * here = opendir(dir);
    
    if (here) {
      fprintf(out,"<p><b><div style=\"font : 40px Verdana Bold\">---- %s ----</div></b><p>\n", dir);

      struct dirent * de;

      while ((de = readdir(here))) {
	if ((string)de->d_name != ".." &&
	    (string)de->d_name != "." &&
	    util::lcase(de->d_name) != "thumbs.db") {
	  
	  string fn = de->d_name;
	  
	  fprintf(out,"<br><a href=\"%s/%s\">%s</a>\n",
		 dir, fn.c_str(), fn.c_str());

	}
      }
    } else {
      fprintf(out,"<p>((couldn't open %s))\n", dir);
    }
    closedir(here);
  }

  printf("</div>\n"
	 "</body></html>\n");

  return 0;

}


/* must end in .??? */
bool nameok(string s) {
  
  if (s.length() >= 4) {
    string y = s.substr(s.length() - 4, 4);
    
    return (y[0] == '.' &&
	    y[1] != '.' &&
	    y[2] != '.' &&
	    y[3] != '.');
  } else return false;
}

string newname(string s) {
  /* find occurrence of .???, erase, tack on tail */
  
  unsigned int idx = 0;
  for (;idx <= s.length() -4; idx++) {
    
    if (s[idx  ] == '.' &&
	s[idx+1] != '.' &&
	s[idx+2] != '.' &&
	s[idx+3] != '.') {
      return s.substr(0, idx) +
	s.substr(idx + 4, s.length() - (idx + 4)) +
	s.substr(idx, 4);
    }

  }

  /* ?? */
  return s;
  
}
