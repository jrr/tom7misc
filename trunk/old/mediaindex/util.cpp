
#include <sys/stat.h>

#include "util.h"
#include "extent.h"

#ifdef WIN32
   /* chdir */
#  include <direct.h>
   /* getpid */
#  include <process.h>
   /* time */
#  include <time.h>
   /* rename */
#  include <io.h>
#else /* posix */
   /* chdir, unlink */
#  include <unistd.h>
   /* getpid */
#  include <sys/types.h>
#endif

struct linereal : public line {
  int x0, y0, x1, y1;
  int dx, dy;
  int stepx, stepy;
  int frac;

  virtual ~linereal() {}
  
  linereal(int x0_, int y0_, int x1_, int y1_) : 
    x0(x0_), y0(y0_), x1(x1_), y1(y1_) {
    

    dy = y1 - y0;
    dx = x1 - x0;

    if (dy < 0) { 
      dy = -dy;
      stepy = -1; 
    } else {
      stepy = 1; 
    }

    if (dx < 0) {
      dx = -dx;
      stepx = -1;
    } else { 
      stepx = 1; 
    }

    dy <<= 1;
    dx <<= 1;

    if (dx > dy) {
      frac = dy - (dx >> 1);
    } else {
      frac = dx - (dy >> 1);
    }
  }

  bool next(int & cx, int & cy) {
    if (dx > dy) {
      if (x0 == x1) return false;
      else {
	if (frac >= 0) {
	  y0 += stepy;
	  frac -= dx;
	}
	x0 += stepx;
	frac += dy;
	cx = x0;
	cy = y0;
	return true;
      }
    } else {
      if (y0 == y1) return false;
      else {
	if (frac >= 0) {
	  x0 += stepx;
	  frac -= dy;
	}
	y0 += stepy;
	frac += dx;
	cx = x0;
	cy = y0;
	return true;
      }
    }
  }
  
  virtual void destroy() {
    delete this;
  }

};

line * line::create(int a, int b, int c, int d) {
  return new linereal(a, b, c, d);
}

bool isdir(string f) {
  struct stat st;
  return (!stat(f.c_str(), &st)) && (st.st_mode & S_IFDIR);
}

bool existsfile(string s) {
  struct stat st;

  return !stat(s.c_str(), &st);
}

bool util::existsdir(string d) {
  return isdir(d); /* (existsfile(d) && isdir(d.c_str())); */
}

/* XXX what mode? */
bool util::makedir(string d) {
# ifdef WIN32
  return !mkdir(d.c_str());
# else /* posix */
  return !mkdir(d.c_str(), 0755);
# endif
}

string readfile(string s) {
  if (isdir(s)) return "";
  
  FILE * f = fopen(s.c_str(), "rb");

  if (!f) return "";
  fseek(f, 0, SEEK_END);
  int size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char * ss = (char*)malloc(size);
  fread(ss, 1, size, f);

  fclose(f);

  string ret = string(ss, size);
  free(ss);

  return ret;
}

string readfilemagic(string s, string mag) {
  if (isdir(s)) return "";
  
  FILE * f = fopen(s.c_str(), "rb");

  if (!f) return "";

  char * hdr = (char*)malloc(mag.length());
  if (!hdr) return "";

  fread(hdr, 1, mag.length(), f);

  for(unsigned int i = 0; i < mag.length(); i++) {
    if (hdr[i] != mag[i]) {
      fclose(f);
      free(hdr);
      return "";
    }
  }
  
  free(hdr);

  /* ok, now just read file */

  fseek(f, 0, SEEK_END);
  int size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char * ss = (char*)malloc(size);
  fread(ss, 1, size, f);

  fclose(f);

  string ret = string(ss, size);
  free(ss);

  return ret;
}

int writefile(string fn, string s) {

  FILE * f = fopen(fn.c_str(), "wb");
  if (!f) return 0;

  fwrite(s.c_str(), 1, s.length(), f);

  fclose(f);
  
  return 1;

}

string itos(int i) {
  char s[64];
  sprintf(s, "%d", i);
  return (string)s;
}

string sizes(int i) {
  string s = "    ";
  s[0] = 255&(i >> 24);
  s[1] = 255&(i >> 16);
  s[2] = 255&(i >> 8);
  s[3] = 255& i;
  return s;
}

/* XXX these have terrible names */

/* represent int i using only b bytes */
string shint(int b, int i) {
  return sizes(i).substr(4-b, b);
}

/* inverse of shint. does not check that
   there is enough room in s to read b bytes
   from idx ... */
int shout(int b, string s, unsigned int & idx) {
  int r = 0;
  while(b--) {
    r = ((unsigned char)s[idx++]) + (r<<8);
  }
  return r;
}

unsigned int util :: hash(string s) {
  unsigned int h = 0x714FA5DD;
  for(unsigned int i = 0; i < s.length(); i ++) {
    h = (h << 11) | (h >> (32 - 11));
    h *= 3113;
    h ^= (unsigned char)s[i];
  }
  return h;
}

string util :: lcase(string in) {
  string out;
  for(unsigned int i = 0; i < in.length(); i++) {
    if (in[i] >= 'A' &&
	in[i] <= 'Z') out += in[i]|32;
    
    else out += in[i];
  }
  return out;
}

string util :: fileof(string s) {
  int i = s.length () - 1;
  for(; i >= 0; i --) {
    if (s[i] == DIRSEPC) {
      return s.substr(i + 1, s.length () - (i + 1));
    }
  }
  return s;
}

string util :: pathof(string s) {
  if (s == "") return ".";
  int i = s.length () - 1;
  for(; i >= 0; i --) {
    if (s[i] == DIRSEPC) {
      return s.substr(0, i);
    }
  }
  return ".";
}

string util :: ensureext(string f, string ext) {
  if (f.length () < ext.length())
    return f + ext;
  else {
    if (f.substr(f.length () - ext.length(),
		 ext.length()) != ext)
      return f + ext;
    else return f;
  }
}

int util :: changedir(string s) {
  return chdir(s.c_str());
}

int util :: getpid() {
  return ::getpid();
}

int stoi(string s) {
  return atoi(s.c_str());
}

/* XXX race. should use creat
   with O_EXCL on unix, at least. */
FILE * util :: open_new(string fname) {
  if (!existsfile(fname))
    return fopen(fname.c_str(), "wb+");
  else return 0;
}

string util :: getline(string & chunk) {
  string ret;
  for(unsigned int i = 0; i < chunk.length(); i ++) {
    if (chunk[i] == '\r') continue;
    else if (chunk[i] == '\n') {
      chunk = chunk.substr(i + 1, chunk.length() - (i + 1));
      return ret;
    } else ret += chunk[i];
  }
  /* there doesn't need to be a final trailing newline. */
  chunk = "";
  return ret;
}

/* PERF use substr instead of accumulating: this is used
   frequently in the net stuff */
/* return first token in line, removing it from 'line' */
string util::chop(string & line) {
  for (unsigned int i = 0; i < line.length(); i ++) {
    if (line[i] != ' ') {
      string acc;
      for(unsigned int j = i; j < line.length (); j ++) {
	if (line[j] == ' ') {
	  line = line.substr(j, line.length() - j);
	  return acc;
	} else acc += line[j];
      }
      line = "";
      return acc;
    }
  }
  /* all whitespace */
  line = "";
  return "";
}

/* PERF same */
string util::chopto(char c, string & line) {
  string acc;
  for (unsigned int i = 0; i < line.length(); i ++) {
    if (line[i] != c) {
      acc += line[i];
    } else {
      if (i < (line.length() - 1)) {
	line = line.substr(i + 1, line.length() - (i + 1));
	return acc;
      } else {
	line = "";
	return acc;
      }
    }
  }
  /* character didn't appear; treat as an invisible
     occurrence at the end */
  line = "";
  return acc;
}

string util::losewhitel(const string & s) {
  for(unsigned int i = 0; i < s.length(); i ++) {
    switch(s[i]) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      /* keep going ... */
      break;
    default:
      return s.substr(i, s.length () - i);
    }
  }
  /* all whitespace */
  return "";
}

string util::tempfile(string suffix) {
  static int tries = 0;

  char * fname = new char[suffix.length() + 128];
  extentda<char> ef(fname);

  do {
    sprintf(fname, 
	    "%d_%d_%d%s", 
	    tries, getpid(), random(),
	    suffix.c_str());
    tries++;
  } while(existsfile(fname));

  return fname;

}

/* try a few methods to remove a file.
   An executable can't remove itself in
   Windows 98, though.
*/
bool util::remove(string f) {
  if (!existsfile(f.c_str())) return true;
  else {
# ifdef WIN32
    /* We can do this by:
       rename tmp  delme1234.exe
       exec(delme1234.exe "-replace" "escape.exe")
          (now, the program has to have a flag -replace
	   that instructs it to replace escape.exe 
	   with itself, then exit)
       .. hopefully exec will unlock the original
       process's executable!! */

    string fname = tempfile(".deleteme");

    /* try unlinking. if that fails,
       rename it away. */
    if (0 == unlink(f.c_str())) return true;
    if (0 == rename(f.c_str(), fname.c_str())) return true;

# else /* posix */
    if (0 == unlink(f.c_str())) return true;
# endif
  } 
  return false;

}

bool util::move(string src, string dst) {
# ifdef WIN32
  if (0 == rename(src.c_str(), dst.c_str()))
    return true;
  else return false;

# else /* posix */
  /* XXX actually, posix has rename too. */
  if (0 == link(src.c_str(), dst.c_str())) {
    /* succeed regardless of whether we
       can remove the old link or not. */
    unlink(src.c_str());
    return true;
  } else return false;
# endif
}


bool util::copy(string src, string dst) {
  FILE * s = fopen(src.c_str(), "rb");
  if (!s) return false;
  FILE * d = fopen(dst.c_str(), "wb+");
  if (!d) { fclose(s); return false; }

  char buf[256];
  int x = 0;
  do {
    /* XXX doesn't distinguish error from EOF, but... */
    x = fread(buf, 1, 256, s);
    if (x) {
      if ((signed)fwrite(buf, 1, x, d) < x) {
	fclose(s);
	fclose(d);
	return false;
      }
    }
  } while (x == 256);

  fclose(s);
  fclose(d);
  return true;
}

void util::toattic(string f) {
  string nf = f;
  /* if it doesn't exist... */
  makedir("attic");
  for(unsigned int i = 0; i < nf.length() ; i ++) {
    if (nf[i] == DIRSEPC) nf[i] = '_';
  }
  /* XXX race */
  int tries = 12;
  while(tries--) {
    string dest = (string)"attic" + DIRSEP + 
                  itos(random()) + (string)"_" + nf;
    if (!existsfile(dest)) {
      move(f, dest);
      return;
    }
  }
}

void util::createpathfor(string f) {
  string s;
  for(unsigned int i = 0; i < f.length();  i++) {
    if (f[i] == DIRSEPC) {
      /* initial / will cause s == "" for first
	 appearance */
      if (s != "") makedir(s);
    }
    s += f[i];
  }
}

FILE * util::fopenp(string f, string m) {
  createpathfor(f);
  return fopen(f.c_str(), m.c_str());
}

/* XXX, could use better source of randomness (kernel)
   on systems that support it. But we don't have any
   real need for cryptographic randomness.

   web sequence numbers are chosen randomly, now, so we
   do. 
*/
int util :: random () {
# ifdef WIN32
  return ::rand();
# else
  return ::random();
# endif
}

/* ensure that random is seeded */
struct randomseed {
  randomseed() {
# ifdef WIN32
    srand(time(0) ^ getpid());
# else
    srandom(time(0) ^ getpid());
# endif
  }
};

randomseed randomseed__unused;

