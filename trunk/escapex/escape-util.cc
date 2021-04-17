
#include <sys/stat.h>
#include <cstring>
#include <cstdint>
#include <string.h>

#include "escape-util.h"

#ifdef WIN32
   /* chdir */
#  include <direct.h>
   /* getpid */
#  include <process.h>
   /* time */
#  include <time.h>
   /* rename */
#  include <io.h>
   /* setclipboard */

// Avoid conflicts with c++17's std::byte
#define byte win_byte_override
#  include <windows.h>
#undef byte

#else /* posix */
   /* chdir, unlink */
#  include <unistd.h>
   /* getpid */
#  include <sys/types.h>
   /* isalnum */
#  include <ctype.h>
#endif

using namespace std;

bool EscapeUtil::isdir(string f) {
  struct stat st;
  return !stat(f.c_str(), &st) && (st.st_mode & S_IFDIR);
}

bool EscapeUtil::existsfile(string s) {
  struct stat st;

  return !stat(s.c_str(), &st);
}

bool EscapeUtil::existsdir(string d) {
  return isdir(d); /* (existsfile(d) && isdir(d.c_str())); */
}

/* XXX what mode? */
bool EscapeUtil::makedir(string d) {
# ifdef WIN32
  return !mkdir(d.c_str());
# else /* posix */
  return !mkdir(d.c_str(), 0755);
# endif
}

static bool hasmagicf(FILE *f, const string &mag) {
  char *hdr = (char*)malloc(mag.length());
  if (!hdr) return false;

  /* we may not even be able to read sizeof(header) bytes! */
  if (mag.length() != fread(hdr, 1, mag.length(), f)) {
    free(hdr);
    return false;
  }

  for (unsigned int i = 0; i < mag.length(); i++) {
    if (hdr[i] != mag[i]) {
      free(hdr);
      return false;
    }
  }

  free(hdr);
  return true;
}

bool EscapeUtil::hasmagic(string s, const string &mag) {
  FILE *f = fopen(s.c_str(), "rb");
  if (!f) return false;

  bool hm = hasmagicf(f, mag);

  fclose(f);
  return hm;
}

string EscapeUtil::readfilemagic(string s, const string &mag) {
  if (isdir(s)) return "";
  if (s == "") return "";

  // printf("opened %s\n", s.c_str());

  /* PERF try this: and see! */
  // printf("Readfile '%s'\n", s.c_str());

  FILE *f = fopen(s.c_str(), "rb");

  if (!f) return "";


  if (!hasmagicf(f, mag)) {
    fclose(f);
    return "";
  }

  /* ok, now just read file */

  fseek(f, 0, SEEK_END);
  int size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char *ss = (char*)malloc(size);
  fread(ss, 1, size, f);

  fclose(f);

  string ret = string(ss, size);
  free(ss);

  return ret;
}

string itos(int i) {
  char s[64];
  sprintf(s, "%d", i);
  return (string)s;
}

string EscapeUtil::ptos(void *p) {
  char s[64];
  sprintf(s, "%p", p);
  return (string)s;
}

unsigned int EscapeUtil::hash(string s) {
  unsigned int h = 0x714FA5DD;
  for (unsigned int i = 0; i < s.length(); i++) {
    h = (h << 11) | (h >> (32 - 11));
    h *= 3113;
    h ^= (unsigned char)s[i];
  }
  return h;
}

string EscapeUtil::lcase(string in) {
  string out;
  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] >= 'A' &&
        in[i] <= 'Z') out += in[i]|32;

    else out += in[i];
  }
  return out;
}

string EscapeUtil::ucase(string in) {
  string out;
  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] >= 'a' &&
        in[i] <= 'z') out += (in[i] & (~ 32));

    else out += in[i];
  }
  return out;
}

string EscapeUtil::fileof(string s) {
  for (int i = s.length() - 1; i >= 0; i--) {
    if (s[i] == DIRSEPC) {
      return s.substr(i + 1, s.length() - (i + 1));
    }
  }
  return s;
}

string EscapeUtil::pathof(string s) {
  if (s == "") return ".";
  for (int i = s.length() - 1; i >= 0; i--) {
    if (s[i] == DIRSEPC) {
      return s.substr(0, i);
    }
  }
  return ".";
}

/* XX can use endswith below */
string EscapeUtil::ensureext(string f, string ext) {
  if (f.length() < ext.length())
    return f + ext;
  else {
    if (f.substr(f.length() - ext.length(),
                 ext.length()) != ext)
      return f + ext;
    else return f;
  }
}

bool EscapeUtil::endswith(string big, string small_) {
  if (small_.length() > big.length()) return false;
  return big.substr(big.length() - small_.length(),
                    small_.length()) == small_;
}

bool EscapeUtil::startswith(string big, string small_) {
  if (small_.length() > big.length()) return false;
  return big.substr(0, small_.length()) == small_;
}

int EscapeUtil::changedir(string s) {
  return !chdir(s.c_str());
}

int EscapeUtil::getpid() {
  return ::getpid();
}

int EscapeUtil::stoi(string s) {
  return atoi(s.c_str());
}

/* XXX race. should use creat
   with O_EXCL on unix, at least. */
FILE *EscapeUtil::open_new(string fname) {
  if (!existsfile(fname))
    return fopen(fname.c_str(), "wb+");
  else return 0;
}

string EscapeUtil::getline(string &chunk) {
  string ret;
  for (unsigned int i = 0; i < chunk.length(); i++) {
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

/* PERF */
string EscapeUtil::fgetline(FILE *f) {
  string out;
  int c;
  while ( (c = fgetc(f)), ((c != EOF) && (c != '\n')) ) {
    /* ignore CR */
    if (c != '\r') {
      out += (char)c;
    }
  }
  return out;
}

/* PERF use substr instead of accumulating: this is used
   frequently in the net stuff */
/* return first token in line, removing it from 'line' */
string EscapeUtil::chop(string &line) {
  for (unsigned int i = 0; i < line.length(); i++) {
    if (line[i] != ' ') {
      string acc;
      for (unsigned int j = i; j < line.length(); j++) {
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
string EscapeUtil::chopto(char c, string &line) {
  string acc;
  for (unsigned int i = 0; i < line.length(); i++) {
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

string EscapeUtil::losewhitel(const string &s) {
  for (unsigned int i = 0; i < s.length(); i++) {
    switch (s[i]) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      /* keep going ... */
      break;
    default:
      return s.substr(i, s.length() - i);
    }
  }
  /* all whitespace */
  return "";
}

string EscapeUtil::tempfile(string suffix) {
  static int tries = 0;

  string fname;
  do {
    char c[128];
    sprintf(c,
            "%d_%d_%d",
            tries, getpid(), random());
    tries++;
    fname = (string)c + suffix;
  } while (existsfile(fname));

  return fname;
}

/* break up the strings into tokens. A token is either
   a single character (non-numeral) or a sequence of
   numerals that are interpreted as a number. We then
   do lexicographic comparison on this stream of tokens.
   assumes ascii.

   l < r     -1
   l = r      0
   l > r      1

   XXX this treats

   abc 0123 def
   abc 123 def

   as equal strings. perhaps don't allow 0 to start a
   number?

   n.b. it is easy to overflow here, so perhaps comparing
   as we go is better
*/
int EscapeUtil::natural_compare(const string &l, const string &r) {
  for (int caseless = 0; caseless < 2; caseless++) {
    unsigned int il = 0;
    unsigned int ir = 0;

    while (il < l.length() || ir < r.length()) {
      /* if out of tokens in either string, it comes first. */
      if (il >= l.length()) return -1;
      if (ir >= r.length()) return 1;

      int lc = (unsigned char)l[il];
      int rc = (unsigned char)r[ir];

      if (lc >= '0' && lc <= '9') {
        if (rc >= '0' && rc <= '9') {
          /* compare ints */
          int ll = 0;
          int rr = 0;

          while (il < l.length() && l[il] >= '0' && l[il] <= '9') {
            ll *= 10;
            ll += (l[il] - '0');
            il++;
          }

          while (ir < r.length() && r[ir] >= '0' && r[ir] <= '9') {
            rr *= 10;
            rr += (r[ir] - '0');
            ir++;
          }

          if (ll < rr) return -1;
          if (ll > rr) return 1;
          /* otherwise continue... */

          il++;
          ir++;
        } else {
          /* treat numbers larger than any char. */
          return 1;
        }
      } else {
        if (rc >= '0' && rc <= '9') {
          return -1;
        } else {
          /* compare chars */
          if ((rc|32) >= 'a' && (rc|32) <= 'z' &&
              (lc|32) >= 'a' && (rc|32) <= 'z' &&
              !caseless) {

            /* letters are case-insensitive */
            if ((lc|32) < (rc|32)) return -1;
            if ((lc|32) > (rc|32)) return 1;
          } else {
            if (lc < rc) return -1;
            if (lc > rc) return 1;
          }

          /* same so far. continue... */

          il++;
          ir++;
        }
      }

    }
    /* strings look equal when compared
       as case-insensitive. so try again
       sensitive */
  }

  /* strings are case-sensitive equal! */

  return 0;
}

/* same as above, but ignore "the" at beginning */
/* XXX also ignore symbols ie ... at the beginning */
int EscapeUtil::library_compare(const string &l, const string &r) {

  /* XXX currently IGNOREs symbols, which could give incorrect
     results for strings that are equal other than their
     leading punctuation */
  unsigned int idxl = 0;
  unsigned int idxr = 0;
  while (idxl < l.length() && (!isalnum(l[idxl]))) idxl++;
  while (idxr < r.length() && (!isalnum(r[idxr]))) idxr++;

  bool thel = false;
  bool ther = false;
  if (l.length() >= (5 + idxl) &&
      (l[idxl + 0]|32) == 't' &&
      (l[idxl + 1]|32) == 'h' &&
      (l[idxl + 2]|32) == 'e' &&
      (l[idxl + 3])    == ' ') thel = true;

  if (r.length() >= (5 + idxr) &&
      (r[idxr + 0]|32) == 't' &&
      (r[idxr + 1]|32) == 'h' &&
      (r[idxr + 2]|32) == 'e' &&
      (r[idxr + 3])    == ' ') ther = true;

  if (thel != ther) {
    if (thel) idxl += 4;
    else idxr += 4;
  }

  return natural_compare(l.substr(idxl, l.length() - idxl),
                         r.substr(idxr, r.length() - idxr));
}

/* XXX impossible to specify a spec for just ^ */
bool EscapeUtil::matchspec(string spec, char c) {
  if (!spec.length()) return false;
  else if (spec[0] == '^')
  return !matchspec(spec.substr(1, spec.length() - 1), c);

  /* now loop looking for c in string, or ranges */
  for (unsigned int i = 0; i < spec.length(); i++) {
    /* ok if starts range, since they are inclusive */
    if (spec[i] == c) return true;

    /* handle ranges */
    if (spec[i] == '-') {
      /* can't be first or last */
      if (i && i < (spec.length() - 1)) {
        if (spec[i - 1] <= c &&
            spec[i + 1] >= c) return true;
        /* skip dash and next char */
        i++;
      }
    }
  }
  return false; /* no match */
}


bool EscapeUtil::library_matches(char k, const string &s) {
  /* skip symbolic */
  unsigned int idx = 0;
  while (idx < s.length() && (!isalnum(s[idx]))) idx++;

  /* skip 'the' */
  if (s.length() >= (idx + 5) &&
      (s[idx]|32) == 't' &&
      (s[idx + 1]|32) == 'h' &&
      (s[idx + 2]|32) == 'e' &&
      (s[idx + 3])    == ' ') return (s[idx + 4]|32) == (k|32);
  else return (s.length() > 0 && (s[idx]|32) == (k|32));
}

/* try a few methods to remove a file.
   An executable can't remove itself in
   Windows 98, though.
*/
bool EscapeUtil::remove(string f) {
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

    /* try unlinking. if that fails,
       rename it away. */
    if (0 == unlink(f.c_str())) return true;

    string fname = tempfile(".deleteme");
    if (0 == rename(f.c_str(), fname.c_str())) return true;

# else /* posix */
    if (0 == unlink(f.c_str())) return true;
# endif
  }
  return false;

}

bool EscapeUtil::move(string src, string dst) {
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
  } else {
    /* try copy and unlink... (link doesn't work on AFS?) */
    if (copy(src, dst)) {
      unlink(src.c_str());
      return true;
    } return false;
  }
# endif
}


bool EscapeUtil::copy(string src, string dst) {
  FILE *s = fopen(src.c_str(), "rb");
  if (!s) return false;
  FILE *d = fopen(dst.c_str(), "wb+");
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

string EscapeUtil::dirplus(const string &dir_, const string &file) {
  if (dir_.empty()) return file;
  if (!file.empty() && file[0] == DIRSEPC) return file;
  string dir = dir_;
  if (dir[dir.size() - 1] != DIRSEPC)
    dir += DIRSEPC;
  return dir + file;
}

void EscapeUtil::toattic(string f) {
  string nf = f;
  // printf("TOATTIC %s...\n", f.c_str());
  /* in case it doesn't exist... */
  makedir(ATTIC_DIR);
  for (unsigned int i = 0; i < nf.length(); i++) {
    if (nf[i] == DIRSEPC) nf[i] = '_';
  }
  /* XXX race */
  int tries = 12;
  while (tries--) {
    string dest = (string)ATTIC_DIR + DIRSEP +
                  itos(random()) + (string)"_" + nf;
    if (!existsfile(dest)) {
      // printf("move(%s,%s)\n", f.c_str(), dest.c_str());
      move(f, dest);
      return;
    }
  }
}

string EscapeUtil::cdup(const string &dir) {
  /* XXX right second argument to rfind? I want to find the last / */
  int idx = dir.rfind(DIRSEP, dir.length() - 1);
  if (idx != (signed)string::npos) {
    if (idx) return dir.substr(0, idx);
    else return ".";
  } else return ".";
}

void EscapeUtil::createpathfor(string f) {
  string s;
  for (unsigned int i = 0; i < f.length();  i++) {
    if (f[i] == DIRSEPC) {
      /* initial / will cause s == "" for first
         appearance */
      if (s != "") makedir(s);
    }
    s += f[i];
  }
}

FILE *EscapeUtil::fopenp(string f, string m) {
  createpathfor(f);
  return fopen(f.c_str(), m.c_str());
}

string EscapeUtil::replace(string src, string findme, string rep) {
  int idx = src.length() - 1;

  if (findme.length() < 1) return src;

  /* idx represents the position in src which, for all chars greater
     than it, there begins no match of findme */
  while (idx >= 0 && idx != (signed)string::npos) {
    idx = src.rfind(findme, idx);
    if (idx != (signed)string::npos) {
      /* do replacement */
      src.replace(idx, findme.length(), rep);
      /* want to ensure termination even if rep contains findmes */
      idx -= findme.length();
    } else break;
  }
  return src;
}

#if WIN32
// for ShellExecute
# include <shellapi.h>
# include <shlobj.h>
#endif

/* return true on success */
bool EscapeUtil::launchurl(const string &url) {
  /* XXX ??? */
#if 0
#ifdef OSX
  CFURLRef urlcfurl = CFURLCreateWithBytes(kCFAllocatorDefault,
                                           (const UInt8*)url.c_str(),
                                           (CFIndex)strlen(urlstring),
                                           kCFStringEncodingASCII, NULL);
  if (urlcfurl) {
      OSStatus status = LSOpenCFURLRef(urlcfurl, NULL);
      CFRelease(urlcfurl);
      return status == noErr;
    }
  return 0;
#endif
#endif

#if WIN32
  // ShellExecute returns an HINSTANCE, but it's really an int. I
  // guess this function call is so old that they needed some 16-bit
  // compatibility. Can't just cast to "int" per Microsoft's
  // documentation because that triggers a loss of precision warning.
  // Fortunately C++11 has a good integral type for this.
  return ((intptr_t)ShellExecute(NULL, "open", url.c_str(),
                                 NULL, NULL, SW_SHOWNORMAL)) > 32;
#endif

  /* otherwise.. */
  return false;
}


float EscapeUtil::randfrac() {
  return random() / (float)RAND_MAX;
}

/* XXX implement */
/* see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/usingtheclipboard.asp ... ugh.

   better: http://www.libsdl.org/projects/scrap/
*/
bool EscapeUtil::setclipboard(string as) {
# ifdef WIN32
  /* handle = hdwp. but how to get one of those? */
  // SetClipboardData(CF_TEXT, as.c_str());
  return false;
  // return true;
# else
  /* how on other platforms? */
  return false;
# endif
}


/* XXX, could use better source of randomness (kernel)
   on systems that support it. But we don't have any
   real need for cryptographic randomness.

   web sequence numbers are chosen randomly, now, so we
   actually do.
*/
int EscapeUtil::random() {
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
    /* run it a bit */
    for (int i = 0; i < 256; i++)
      (void)EscapeUtil::random();
  }
};

randomseed randomseed__unused;

