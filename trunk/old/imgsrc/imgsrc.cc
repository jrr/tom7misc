
/* imgsrc scans a list of html files, adding height= and width= tags where
   appropriate and possible, and leaving the rest of the file unchanged.

   This program comes with no warranty and its distribution is subject to
   the Gnu Public License. (http://www.gnu.org/copyleft/gpl.html)

   Written by Tom Murphy 7 in 1999. This definitely does not handle all
   possible html documents properly -- if your code is strange or
   invalid, this could potentially corrupt your files. Back up before
   running it!

   On 4.25.99, added a command parameter -r (path) to specify document
   root so <img src=/images/dot.gif> works.

*/


#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include <unistd.h>
#include <string>

/* portions derived from IJG code */

int gifsize(const char * name, int & x, int & y);
int jpgsize(const char * name, int & x, int & y);
int scanhead (FILE * infile, int & image_width, int & image_height);

string pathfix(const char * name);

int process(FILE*,FILE*);
void abort(FILE*,FILE*);

string docroot; /* prepend to non-relative paths */

int main (int argc, char ** argv) {
  char ** argvv = argv;
  if (argc < 2) {
    printf("%s: give file or list of files on command line\n", *argvv);
    exit(0);
  }

  #ifdef DJGPP
    putenv("LFN=y"); // enable long filename support under djgpp
  #endif
  
  argv++;
  for (;--argc;) {
    if (**argv == '-') {
      /* command switch */
      
      if (!strcasecmp(*argv,"-r")) {
	argv++;
	
	docroot = *argv;

	/* strip possible trailing / */
	if (docroot[docroot.length()-1] == '/'
	  ||docroot[docroot.length()-1] == '\\') {
	  docroot = docroot.substr(0,docroot.length()-1);
	}

	argc--;
	argv++;
	continue;
      } else {
	printf("Unknown switch %s. Abort.\n", *argv);
	exit(-1);
      }

    }
    if (rename(*argv,"#IMGSRC#")) {
      printf("%s: failed to rename %s\n", argvv[0], *argv);
      argv++;
      continue;
    }

    FILE * inways=0, * outie=0;

    if (! (( inways = fopen("#IMGSRC#","rb+"))
	   && (outie = fopen(*argv,"wb+")))) {
      printf("%s: Couldn't open the files!\n", *argvv);
      exit(-1);
    }

    process(inways,outie);


    fclose(inways);
    fclose(outie);
    
    if (unlink("#IMGSRC#")) {
      printf("%s: Warning: Couldn't delete temporary file\n",*argvv);
    }
    argv++;
  }
}

int gifsize(const char * name, int & x, int & y) {

  /* hash, cache */
  string s;
  FILE * gif = fopen((s = pathfix(name)).c_str(),"rb");
  if (!gif) {
    printf("Warning: can't size %s\n", s.c_str());
    return 0;
  }
  
  int no=6;
  while (no--) fgetc(gif);

  x = y = 0;

  x = fgetc(gif);
  x |= fgetc(gif) << 8;

  y = fgetc(gif);
  y |= fgetc(gif) << 8;

  fclose(gif);

  printf("[%s] sizes to:  %d %d\n",name,x,y);
 
  return 1;

}

int jpgsize(const char * name, int & x, int & y) {


  string s;
  FILE * jpg = fopen((s = pathfix(name)).c_str(),"rb");
  if (!jpg) {
    printf("Warning: can't size %s\n", s.c_str());
    return 0;
  }

  int t = scanhead(jpg,x,y);
  
  printf("[%s] sizes to:  %d %d\n",name,x,y);
  
  fclose(jpg);

  return t;
}

/* no repeated chars */
const char * FSM = "<img";
const char * FSM2= "src=";
const char * FSM3= "width=";

int process(FILE * inways, FILE * outie) {
  
  int c;
  int f=0;

  while (EOF != (c = getc(inways))) {

    if ((c|32)==FSM[f]) f++; else if ((c|32)==FSM[f=0]) f++;
    
    if (!FSM[f]) { /* responsible for putting char */
      /* img */
      /* find 'src=' */
      fputc(c,outie);

      int d=0;
      int writesize=1;
      int v=0;

      while (EOF != (c=getc(inways))) {

	if (c=='>') { 
	  fputc(c,outie);
	  printf("Warning: no SRC in IMG tag (abort)\n");
	  abort(inways,outie);
	  return 1;
	}

	if ((c|32)==FSM3[v]) v++; else if ((c|32)==FSM3[v=0]) v++;
	if (!FSM3[v]) {
	  writesize=0;
	  v=0;
	}

	
	if ((c|32)==FSM2[d]) d++; else if ((c|32)==FSM2[d=0]) d++;
	
	if (!FSM2[d]) {
	  /* src= */
	  fputc(c,outie);
	  string fname;
	  
	  while (EOF != (c=getc(inways))) {
	    if (c==' '|| c=='\n' || c=='\r' || c=='>') {
	      /* possibly strip quotes */
	      if (fname[0] == '\"') {
		fname = fname.substr(1,fname.length()-2);
	      }
	      
	      int xsize=-1, ysize=-1;

	      string ext;
	      for (unsigned int ii=0;ii<fname.length();ii++)
		if (fname[ii] == '.') ext="";
		else ext += char(32|fname[ii]);

	      if (ext == "gif") {
		if (writesize) writesize = gifsize(fname.c_str(),xsize,ysize);
	      } else if (ext == "jpg") {
		if (writesize) writesize = jpgsize(fname.c_str(),xsize,ysize);
	      } else printf("Don't know how to size .%s files.\n", ext.c_str());
       
	      v=0;
	      do {

		if (c=='>') {
		  if (writesize) {
		    fprintf(outie," width=%d height=%d>", xsize, ysize);
		  } else fputc('>',outie);
		  f=0; goto done;
		} else {
		  
		  if ((c|32)==FSM3[v]) v++; else if ((c|32)==FSM3[v=0]) v++;
		  if (!FSM3[v]) {
		    writesize=0;
		    v=0;
		  }

		  fputc(c,outie);

		}
		
	      } while (EOF != (c=getc(inways)));

	    } else { fname += (char)c; fputc(c,outie); }

	  } 

	} else fputc(c,outie);

      }

      
    } else fputc(c,outie);
  done:;
  }
  return 0;
}

void abort (FILE * inways, FILE * outie) {
  int c;
    while (EOF != (c = getc(inways)))
      fputc(c,outie);
}


#define readbyte(a,b) do if(((a)=getc((b))) == EOF) return 0; while (0)
#define readword(a,b) do { int cc_=0,dd_=0; \
                          if((cc_=getc((b))) == EOF \
        		  || (dd_=getc((b))) == EOF) return 0; \
                          (a) = (cc_<<8) + (dd_); \
                          } while(0)

int scanhead (FILE * infile, int & image_width, int & image_height) {
  int marker=0;
  int dummy=0;
  if ( getc(infile) != 0xFF || getc(infile) != 0xD8 )
    return 0;

  for (;
      ;) {

    int discarded_bytes=0;
    readbyte(marker,infile);
    while (marker != 0xFF) {
      discarded_bytes++;
      readbyte(marker,infile);
    }
    do readbyte(marker,infile); while (marker == 0xFF);

    if (discarded_bytes != 0) return 0;
   
    switch (marker) {
    case 0xC0:
    case 0xC1:
    case 0xC2:
    case 0xC3:
    case 0xC5:
    case 0xC6:
    case 0xC7:
    case 0xC9:
    case 0xCA:
    case 0xCB:
    case 0xCD:
    case 0xCE:
    case 0xCF: {
      readword(dummy,infile);	/* usual parameter length count */
      readbyte(dummy,infile);
      readword(image_height,infile);
      readword(image_width,infile);
      readbyte(dummy,infile);
      //      printf("JPEG image is %uw * %uh", image_width, image_height);
      return 1;
      break;
      }
    case 0xDA:
    case 0xD9:
      return 0;
    default: {
	int length;
	
	readword(length,infile);

	if (length < 2)
	  return 0;
	length -= 2;
	while (length > 0) {
	  readbyte(dummy, infile);
	  length--;
	}
      }
      break;
    }
  }
}

string pathfix(const char * n) {

  if (*n == '/') {
    return docroot + (string)n;
  } else return (string)n;
}
