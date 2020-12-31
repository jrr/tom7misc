
#include <sys/stat.h>
#include <cstring>
#include <string.h>
#include <algorithm>
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>
#include <type_traits>
#include <string_view>
#include <optional>

#include "util.h"

#if defined(WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
   /* chdir */
#  include <direct.h>
   /* getpid */
#  include <process.h>
   /* time */
#  include <time.h>
   /* rename */
#  include <io.h>
   /* setclipboard */

// For C++17, avoid conflicts with std::byte
#define byte win_byte_override
#  include <windows.h>
#undef byte

#if defined(__MINGW32__) || defined(__MINGW64__)
#  include <dirent.h>
#endif

// Visual studio only.
#if defined(WIN32) && !defined(__MINGW32__) && !defined(__MINGW64__)
# pragma warning(disable: 4996)
#endif

#else /* posix */
   /* chdir, unlink */
#  include <unistd.h>
   /* getpid */
#  include <sys/types.h>
   /* isalnum */
#  include <ctype.h>
   /* directory stuff */
#  include <dirent.h>
#endif

using namespace std;

using uint8 = uint8_t;
using int64 = int64_t;
using uint64 = uint64_t;

string itos(int i) {
  char s[64];
  sprintf(s, "%d", i);
  return (string)s;
}

string dtos(double d) {
  char s[64];
  sprintf(s, "%.2f", d);
  return (string)s;
}

// TODO: I never tested this on posix.
vector<string> Util::ListFiles(const string &s) {
  vector<string> v;
  DIR *dir = opendir(s.c_str());
  if (dir == nullptr) return {};
  while (struct dirent *res = readdir(dir)) {
    string s = res->d_name;
    if (s != "." && s != "..") {
      v.push_back(std::move(s));
    }
  }
  closedir(dir);
  return v;
}

bool Util::IsHexDigit(char c) {
  return (c >= '0' && c <= '9') ||
    ((c | 32) >= 'a' && (c | 32) <= 'f');
}
int Util::HexDigitValue(char c) {
  // One weird trick.
  return ((int)c | 4400) % 55;
}


namespace {
struct LineReal : public line {
  int x0, y0, x1, y1;
  int dx, dy;
  int stepx, stepy;
  int frac;

  LineReal(int x0_, int y0_, int x1_, int y1_) :
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

  bool next(int &cx, int &cy) {
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

}  // namespace

line *line::create(int a, int b, int c, int d) {
  return new LineReal(a, b, c, d);
}

bool Util::isdir(const string &f) {
  struct stat st;
  return (0 == stat(f.c_str(), &st)) && (st.st_mode & S_IFDIR);
}

bool Util::ExistsFile(const string &s) {
  struct stat st;

  return 0 == stat(s.c_str(), &st);
}

bool Util::existsdir(const string &d) {
  return isdir(d); /* (ExistsFile(d) && isdir(d.c_str())); */
}

/* XXX what mode? */
bool Util::MakeDir(const string &d) {
# if defined(WIN32) || defined(__MINGW32__)
  return !mkdir(d.c_str());
# else /* posix */
  return !mkdir(d.c_str(), 0755);
# endif
}

string Util::ptos(void *p) {
  char s[64];
  sprintf(s, "%p", p);
  return (string)s;
}

// Internal helper used by ReadFile, ReadFileMagic, ReadFileBytes.
// T is string or vector<uint8>.
// If magic_opt is non-null, it is copied to the start of the
// container we return.
template<class T>
static T ReadAndCloseFile(FILE *f, const T *magic_opt) {
  #define READFILE_DEBUG 0
  // This is unbelievably difficult!
  // A simple loop while getc() doesn't return EOF works, but
  // is much slower than it needs to be.
  // fseek(.., SEEK_END) is useless because it has undefined
  // behavior (!) despite "usually" working.
  // stat() returns a size, but it is bogus for special files,
  // like the ones in /proc. It should be accurate for regular
  // files.
  // Therefore, what we want to do here is use stat() to
  // estimate the file size, and be efficient in the case that
  // we got it right. However, we need to remain correct even
  // when stat returns nonsense (such as 0 or 4096).

  // TODO: Not sure how portable fstat64 is. OS X marks it as
  // deprecated since 10.6. An alternative is to compile with
  // -D_FILE_OFFSET_BITS=64. Another alternative is to only do
  // gigabyte-size reads, although reading a file that's a significant
  // fraction of all available memory is one of the main points of
  // going through all this complexity!
  int fd = fileno(f);
  struct stat64 st;
  if (0 != fstat64(fd, &st)) {
    fclose(f);
    return {};
  }

  #if READFILE_DEBUG
  printf("size_t is %d bytes, signed: %s\n",
	 sizeof (size_t), std::is_signed<size_t>::value ? "yes" : "no");
  printf("stat %d:\n"
	 "  st_size: %llu\n (off_t is %d bytes, signed: %s)\n",
	 fd, (uint64)st.st_size, sizeof (st.st_size),
	 std::is_signed<decltype (st.st_size)>::value ? "yes" : "no");
  #endif
  
  T ret;
  int64 next_pos = 0;
  int64 size_guess = st.st_size;
  if (magic_opt != nullptr) {
    ret = *magic_opt;
    next_pos = ret.size();
  }

  // On cygwin for huge files, we get the size correct and read all
  // the bytes in a single call to fread. But feof doesn't return true
  // right after this, which causes us to do a useless resize.
  // Instead, just attempt to read one additional byte, which causes
  // fread to recognize the EOF. (This assumes that resizing the
  // string downward by one byte at the end is basically free.)
  size_guess++;

  #if READFILE_DEBUG
  printf("next_pos is %lld; size_guess is %lld.\n",
	 next_pos, size_guess);
  #endif

  // In optimistic cases where the size_guess is correct,
  // this loop will execute just once: A single resize, and
  // a single fread.
  for (;;) {
    // Now, repeatedly, resize the string to accommodate a
    // read that we think will finish off the file. But
    // don't do zero-sized writes!
    if (next_pos >= size_guess) {
      // XXX possibility for overflow, ugh
      size_guess = next_pos + 16;
    }
    #if READFILE_DEBUG
    printf("Resize buffer to %lld\n", size_guess);
    #endif

    // Keep the buffer large enough to store what we think the actual
    // size is, so that we don't have to keep resizing it.
    if ((int64)ret.size() < size_guess) {
      #if READFILE_DEBUG
      printf("Resize buffer %lld -> %lld\n", (int64)ret.size(), size_guess);
      #endif
      ret.resize(size_guess);
    } else {
      #if READFILE_DEBUG
      printf("Buffer sized %lld; already big enough for guess %lld\n",
	     (int64)ret.size(), size_guess);
      #endif
    }

    // TODO: Tune this for various platforms to find a good
    // compromise. Could even be platform dependent. For corretness,
    // this should not be any larger than 2^31 - 1 (or so), since for
    // example on mingw, fread just fails for such sizes. Huge reads
    // seem to be pretty slow (perhaps preventing scheduling of other
    // threads).
    static constexpr int64 MAX_READ_SIZE = 1LL << 24;
    static_assert(MAX_READ_SIZE < ((1LL << 31) - 1),
		  "Must fit in signed 32-bit gamut.");
    
    const int64 read_size =
      std::min(size_guess - next_pos, MAX_READ_SIZE);
    #if READFILE_DEBUG
    printf("Attempt to read %lld bytes\n", read_size);
    #endif
    
    // Bytes are required to be contiguous from C++11;
    // use .front() instead of [next_pos] since the former,
    // introduced in C++11, will prove we have a compatible
    // version.
    const size_t bytes_read =
      fread(&ret.front() + next_pos, 1, read_size, f);
    #if READFILE_DEBUG
    printf("%lld bytes were read\n", (int64)bytes_read);
    #endif
    
    // We read exactly this many bytes.
    next_pos += bytes_read;
    #if READFILE_DEBUG
    printf("Now next_pos is %lld\n", next_pos);
    #endif
    if (feof(f)) {
      #if READFILE_DEBUG
      printf("EOF. current ret size is %lld; resizing to %lld\n",
	     (int64)ret.size(), next_pos);
      #endif
      // Should be no-op when we guessed correctly.
      ret.resize(next_pos);
      fclose(f);
      return ret;
    }

    // If we're not at the end of file but no bytes were
    // read, then something is amiss. This also ensures
    // that the loop makes progress.
    if (bytes_read == 0) {
      #if READFILE_DEBUG
      printf("No bytes read but not EOF?\n");
      #endif
      fclose(f);
      return {};
    }
  }
}

std::optional<string> Util::ReadFileOpt(const string &s) {
  if (Util::isdir(s)) return nullopt;
  if (s.empty()) return nullopt;
  FILE *f = fopen(s.c_str(), "rb");
  if (f == nullptr) return nullopt;

  // TODO: Some failures are possible in here; should return nullopt for
  // those as well.
  // TODO PERF: Make sure we aren't unnecessarily copying here?
  return {ReadAndCloseFile<string>(f, nullptr)};
}

string Util::ReadFile(const string &s) {
  if (Util::isdir(s)) return "";
  if (s == "") return "";

  FILE *f = fopen(s.c_str(), "rb");
  if (!f) return "";
  return ReadAndCloseFile<string>(f, nullptr);
}

// PERF: Benchmark against ForEachLine approach.
vector<string> Util::ReadFileToLines(const string &f) {
  return SplitToLines(ReadFile(f));
}

bool Util::WriteLinesToFile(const string &fn,
			    const std::vector<string> &lines) {
  FILE *f = fopen(fn.c_str(), "wb");
  if (f == nullptr) return false;

  for (const string &s : lines) {
    size_t len = s.size();
    size_t wrote_len = fwrite(s.c_str(), 1, len, f);
    if (len != wrote_len) {
      fclose(f);
      return false;
    }
    if (EOF == fputc('\n', f)) {
      fclose(f);
      return false;
    }
  }

  fclose(f);

  return true;
}

vector<string> Util::SplitToLines(const string &s) {
  vector<string> v;
  string line;
  // PERF don't need to do so much copying.
  for (size_t i = 0; i < s.size(); i++) {
    if (s[i] == '\r') {
      continue;
    } else if (s[i] == '\n') {
      v.push_back(std::move(line));
      line.clear();
    } else {
      line += s[i];
    }
  }
  return v;
}

map<string, string> Util::ReadFileToMap(const string &f) {
  map<string, string> m;
  vector<string> lines = ReadFileToLines(f);
  for (int i = 0; i < (int)lines.size(); i++) {
    string rest = lines[i];
    string tok = chop(rest);
    rest = losewhitel(rest);
    m.insert(make_pair(tok, rest));
  }
  return m;
}

static inline string PadWith(int n, string s, char c) {
  if (n >= 0) {
    if ((int)s.length() < n) {
      s.reserve(n);
      while ((int)s.length() < n)
	s.push_back(c);
    }
    return s;
  } else {
    // n < 0. Pad left, but negate n so it's easier to think about.
    n = -n;
    if ((int)s.length() < n) {
      string ret(n - s.length(), c);
      ret.append(s);
      return ret;
    } else {
      return s;
    }
  }
}

string Util::Pad(int n, string s) {
  return PadWith(n, std::move(s), ' ');
}

string Util::PadEx(int n, string s, char c) {
  return PadWith(n, std::move(s), c);
}

vector<uint8> Util::ReadFileBytes(const string &s) {
  if (Util::isdir(s)) return {};
  if (s == "") return {};

  FILE *f = fopen(s.c_str(), "rb");
  if (!f) return {};
  return ReadAndCloseFile<vector<uint8>>(f, nullptr);
}


static bool HasMagicF(FILE *f, const string &mag) {
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

bool Util::HasMagic(string s, const string &mag) {
  FILE *f = fopen(s.c_str(), "rb");
  if (!f) return false;

  bool hm = HasMagicF(f, mag);

  fclose(f);
  return hm;
}

string Util::ReadFileMagic(string s, const string &mag) {
  if (isdir(s)) return "";
  if (s == "") return "";

  FILE *f = fopen(s.c_str(), "rb");

  if (!f) return "";


  if (!HasMagicF(f, mag)) {
    fclose(f);
    return "";
  }

  // OK, now just read file.
  return ReadAndCloseFile<string>(f, &mag);
}

bool Util::WriteFile(const string &fn, const string &s) {
  FILE *f = fopen(fn.c_str(), "wb");
  if (f == nullptr) return false;

  const size_t len = s.length();
  const size_t wrote_len = fwrite(s.c_str(), 1, len, f);

  fclose(f);

  return len == wrote_len;
}

bool Util::WriteFileBytes(const string &fn,
			  const vector<uint8> &bytes) {
  FILE *f = fopen(fn.c_str(), "wb");
  if (!f) return false;

  const size_t len = bytes.size();
  const size_t wrote_len = fwrite(&bytes[0], 1, len, f);

  fclose(f);

  return len == wrote_len;
}

vector<uint64> Util::ReadUint64File(const string &filename) {
  vector<uint8> bytes = ReadFileBytes(filename);
  if (bytes.size() & 7) return {};
  vector<uint64> ret;
  ret.reserve(bytes.size() / 8);
  uint64 w = 0ULL;
  for (int i = 0; i < (int)bytes.size(); i++) {
    w <<= 8;
    w |= bytes[i];
    if ((i & 7) == 7) {
      ret.push_back(w);
      w = 0ULL;
    }
  }
  return ret;
}

bool Util::WriteUint64File(const string &filename,
			   const std::vector<uint64> &contents) {
  vector<uint8> bytes;
  bytes.reserve(contents.size() * 8);
  for (uint64 w : contents) {
    bytes.push_back(0xFF & (w >> 56));
    bytes.push_back(0xFF & (w >> 48));
    bytes.push_back(0xFF & (w >> 40));
    bytes.push_back(0xFF & (w >> 32));
    bytes.push_back(0xFF & (w >> 24));
    bytes.push_back(0xFF & (w >> 16));
    bytes.push_back(0xFF & (w >> 8));
    bytes.push_back(0xFF & (w));
  }
  return WriteFileBytes(filename, bytes);
}


string Util::sizes(int i) {
  string s = "    ";
  s[0] = 255&(i >> 24);
  s[1] = 255&(i >> 16);
  s[2] = 255&(i >> 8);
  s[3] = 255& i;
  return s;
}

/* XXX these have terrible names */

/* represent int i (as i mod (2^(b/8)))
   using only b bytes */
string Util::shint(int b, int i) {
  return sizes(i).substr(4-b, b);
}

/* inverse of shint. does not check that
   there is enough room in s to read b bytes
   from idx ... */
int Util::shout(int b, string s, unsigned int &idx) {
  int r = 0;
  while (b--) {
    r = ((unsigned char)s[idx++]) + (r<<8);
  }
  return r;
}

unsigned int Util::hash(const string &s) {
  unsigned int h = 0x714FA5DD;
  for (unsigned int i = 0; i < s.length(); i ++) {
    h = (h << 11) | (h >> (32 - 11));
    h *= 3113;
    h ^= (unsigned char)s[i];
  }
  return h;
}

string Util::lcase(const string &in) {
  string out;
  out.reserve(in.size());
  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] >= 'A' &&
	in[i] <= 'Z') out += in[i]|32;

    else out += in[i];
  }
  return out;
}

string Util::ucase(const string &in) {
  string out;
  out.reserve(in.size());
  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] >= 'a' &&
	in[i] <= 'z') out += (in[i] & (~ 32));

    else out += in[i];
  }
  return out;
}

string Util::fileof(const string &s) {
  for (long long int i = s.length() - 1; i >= 0; i --) {
    if (s[i] == DIRSEPC) {
      return s.substr(i + 1, s.length() - (i + 1));
    }
  }
  return s;
}

string Util::pathof(const string &s) {
  if (s == "") return ".";
  for (long long int i = s.length() - 1; i >= 0; i --) {
    if (s[i] == DIRSEPC) {
      return s.substr(0, i);
    }
  }
  return ".";
}

/* XX can use EndsWith below */
string Util::ensureext(string f, string ext) {
  if (f.length() < ext.length())
    return f + ext;
  else {
    if (f.substr(f.length() - ext.length(),
		 ext.length()) != ext)
      return f + ext;
    else return f;
  }
}

bool Util::EndsWith(string_view big, string_view little) {
  // In c++20, can use s->ends_with(suffix).
  if (big.size() < little.size()) return false;
  return big.substr(big.size() - little.size()) == little;
}

bool Util::StartsWith(string_view big, string_view little) {
  // In c++20, can use s->starts_with(suffix).
  if (big.size() < little.size()) return false;
  return big.substr(0, little.size()) == little;
}

bool Util::TryStripSuffix(string_view suffix, string_view *s) {
  if (EndsWith(*s, suffix)) {
    s->remove_suffix(suffix.length());
    return true;
  }
  return false;
}

bool Util::TryStripSuffix(string_view suffix, string *s) {
  if (EndsWith(*s, suffix)) {
    s->resize(s->size() - suffix.length());
    return true;
  }
  return false;
}

bool Util::TryStripPrefix(string_view prefix, string_view *s) {
  if (StartsWith(*s, prefix)) {
    s->remove_prefix(prefix.length());
    return true;
  }
  return false;
}

bool Util::TryStripPrefix(string_view prefix, string *s) {
  if (StartsWith(*s, prefix)) {
    *s = s->substr(prefix.length(), string::npos);
    return true;
  }
  return false;
}

int Util::changedir(string s) {
  return !chdir(s.c_str());
}

int Util::getpid() {
  return ::getpid();
}

int stoi(const string &s) {
  return atoi(s.c_str());
}

/* XXX race. should use creat
   with O_EXCL on unix, at least. */
FILE *Util::open_new(string fname) {
  if (!ExistsFile(fname))
    return fopen(fname.c_str(), "wb+");
  else return 0;
}

string Util::getline(string &chunk) {
  string ret;
  for (unsigned int i = 0; i < chunk.length(); i ++) {
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
string Util::fgetline(FILE *f) {
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
string Util::chop(string &line) {
  for (unsigned int i = 0; i < line.length(); i ++) {
    if (line[i] != ' ') {
      string acc;
      for (unsigned int j = i; j < line.length(); j ++) {
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

double Util::ParseDouble(const string &s, double default_value) {
  // To get rid of leading and trailing whitespace. strtod will skip
  // it anyway, but we want to be able to check that the whole
  // string was consumed in a simple way.
  string ss = NormalizeWhitespace(s);
  char *endptr = nullptr;
  double d = strtod(ss.c_str(), &endptr);
#if 0
  printf("[%s] %p + %zu = %p vs %p\n",
	 ss.c_str(),
	 ss.c_str(), ss.size(), ss.c_str() + ss.size(),
	 endptr);
#endif
  if (endptr == ss.c_str() + ss.size()) {
    return d;
  } else {
    return default_value;
  }
}

/* PERF same */
string Util::chopto(char c, string &line) {
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

string Util::losewhitel(const string &s) {
  for (unsigned int i = 0; i < s.length(); i ++) {
    switch(s[i]) {
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

string Util::LoseWhiteR(string s) {
  while (!s.empty()) {
    switch (s.back()) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      s.resize(s.size() - 1);
      break;
    default:
      return s;
    }
  }
  // All whitespace.
  return "";
}

string Util::NormalizeWhitespace(const string &s) {
  string ret;
  // Skip at beginning.
  bool skip_ws = true;
  for (char c : s) {
    switch (c) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      if (skip_ws) continue;
      ret += ' ';
      skip_ws = true;
      break;
    default:
      skip_ws = false;
      ret += c;
    }
  }

  return LoseWhiteR(ret);
}

string Util::tempfile(const string &suffix) {
  static int tries = 0;

  char *fname = new char[suffix.length() + 128];

  do {
    sprintf(fname,
	    "%d_%d_%d%s",
	    tries, getpid(), random(),
	    suffix.c_str());
    tries++;
  } while (ExistsFile(fname));

  string ret = fname;
  delete[] fname;
  return ret;
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
int Util::natural_compare(const string &l, const string &r) {

  for (int caseless = 0; caseless < 2; caseless ++) {

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
	    il ++;
	  }

	  while (ir < r.length() && r[ir] >= '0' && r[ir] <= '9') {
	    rr *= 10;
	    rr += (r[ir] - '0');
	    ir ++;
	  }

	  if (ll < rr) return -1;
	  if (ll > rr) return 1;
	  /* otherwise continue... */

	  il ++;
	  ir ++;
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

	  il ++;
	  ir ++;
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
int Util::library_compare(const string &l, const string &r) {

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

  return natural_compare (l.substr(idxl, l.length() - idxl),
			  r.substr(idxr, r.length() - idxr));
}

/* XXX impossible to specify a spec for just ^ */
bool Util::matchspec(string spec, char c) {
  if (!spec.length()) return false;
  else if (spec[0] == '^')
  return !matchspec(spec.substr(1, spec.length() - 1), c);

  /* now loop looking for c in string, or ranges */
  for (unsigned int i = 0; i < spec.length(); i ++) {
    /* ok if starts range, since they are inclusive */
    if (spec[i] == c) return true;

    /* handle ranges */
    if (spec[i] == '-') {
      /* can't be first or last */
      if (i && i < (spec.length() - 1)) {
	if (spec[i - 1] <= c &&
	    spec[i + 1] >= c) return true;
	/* skip dash and next char */
	i ++;
      }
    }
  }
  return false; /* no match */
}


bool Util::library_matches(char k, const string &s) {
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

   XXX Remove escape-specific logic in here.
   Can just use remove from stdio.h
   
*/
bool Util::remove(const string &f) {
  if (!ExistsFile(f.c_str())) return true;
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

bool Util::move(const string &src, const string &dst) {
# if defined(WIN32) || defined(__MINGW32__)
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


bool Util::copy(const string &src, const string &dst) {
  FILE *s = fopen(src.c_str(), "rb");
  if (!s) {
    // fprintf(stderr, "Couldn't open %s for reading\n", src.c_str());
    return false;
  }
  FILE *d = fopen(dst.c_str(), "wb");
  if (!d) {
    // fprintf(stderr, "Couldn't open %s for writing\n", dst.c_str());
    fclose(s);
    return false;
  }

  static constexpr int BUF_SIZE = 16384;
  uint8 buf[BUF_SIZE];
  int x = 0;
  do {
    /* XXX doesn't distinguish error from EOF, but... */
    x = (int)fread(buf, 1, BUF_SIZE, s);
    if (x > 0) {
      if ((signed)fwrite(buf, 1, x, d) < x) {
	fclose(s);
	fclose(d);
	return false;
      }
    }
  } while (x == BUF_SIZE);

  fclose(s);
  fclose(d);
  return true;
}

string Util::dirplus(const string &dir_, const string &file) {
  if (dir_.empty()) return file;
  if (!file.empty() && file[0] == DIRSEPC) return file;
  string dir = dir_;
  if (dir[dir.size() - 1] != DIRSEPC)
    dir += DIRSEPC;
  return dir + file;
}

string Util::cdup(const string &dir) {
  /* Find the last / */
  size_t idx = dir.rfind(DIRSEP, dir.length() - 1);
  if (idx != string::npos) {
    if (idx) return dir.substr(0, idx);
    else return ".";
  } else return ".";
}

void Util::CreatePathFor(const string &f) {
  string s;
  for (unsigned int i = 0; i < f.length();  i++) {
    if (f[i] == DIRSEPC) {
      /* initial / will cause s == "" for first
	 appearance */
      if (s != "") MakeDir(s);
    }
    s += f[i];
  }
}

FILE *Util::fopenp(const string &f, const string &m) {
  CreatePathFor(f);
  return fopen(f.c_str(), m.c_str());
}

string Util::Join(const vector<string> &parts,
		  const string &sep) {
  if (parts.empty()) return "";
  if (parts.size() == 1) return parts[0];
  size_t result_len = 0;
  for (const string &part : parts)
    result_len += part.size();
  result_len += sep.size() * (parts.size() - 1);

  string out;
  out.reserve(result_len);
  out += parts[0];
  // PERF could blit directly and avoid += capacity checks?
  for (size_t i = 1; i < parts.size(); i++) {
    out += sep;
    out += parts[i];
  }
  return out;
}

vector<string> Util::Split(const string &s, char sep) {
  vector<string> out;
  int64 start = 0;
  for (int64 i = 0; i < (int64)s.size(); i++) {
    if (s[i] == sep) {
      // Substring constructor.
      out.emplace_back(s, start, i - start);
      start = i + 1;
      i++;
    }
  }
  out.emplace_back(s, start, string::npos);
  return out;
}

string Util::Replace(string src,
		     const string &findme,
		     const string &rep) {
  auto idx = src.length() - 1;

  if (findme.length() < 1) return src;

  // PERF: Do this in one pass (copying) instead of repeatedly calling replace,
  // which does n^2 work.
  
  /* idx represents the position in src which, for all chars greater
     than it, there begins no match of findme */
  for (;;) {
    idx = src.rfind(findme, idx);
    if (idx == string::npos)
      break;
    /* do replacement */
    src.replace(idx, findme.length(), rep);
    /* don't allow any matches to extend into the string we just inserted;
       (consider replacing "abc" with "bcd" in the string "aabc") */
    if (findme.length() > idx)
      break;
    idx -= findme.length();
  }
  return src;
}

vector<int> Util::Factorize(int n) {
  // Bad input.
  if (n <= 1) return {};
  
  vector<int> out;

  // Reduce twos. Because the input is positive, we'll
  // eventually get a one bit.
  while (0 == (n & 1)) {
    out.push_back(2);
    n >>= 1;
  }

  // Factor to try. Once we eliminate a factor there's no
  // reason to ever try it again, so 
  int f = 3;
  
  while (f * f <= n) {
    while (n % f == 0) {
      out.push_back(f);
      n /= f;
    }
    
    f += 2;
  }

  // Last factor is prime.
  if (n != 1) out.push_back(n);

  std::sort(out.begin(), out.end());
  return out;
}

#ifdef WIN32
// for ShellExecute
# include <shellapi.h>
# include <shlobj.h>
#endif

/* return true on success */
bool Util::launchurl(const string &url) {
  /* XXX ??? */
#if 0
#ifdef OSX
  CFURLRef urlcfurl = CFURLCreateWithBytes(kCFAllocatorDefault,
					   (const UInt8*)url.c_str(),
					   (CFIndex)strlen(urlstring),
					   kCFStringEncodingASCII, nullptr);
  if (urlcfurl) {
      OSStatus status = LSOpenCFURLRef(urlcfurl, nullptr);
      CFRelease(urlcfurl);
      return (status == noErr);
    }
  return 0;
#endif
#endif

#ifdef WIN32
  return ((size_t)ShellExecute(nullptr, "open", url.c_str(),
			       nullptr, nullptr, SW_SHOWNORMAL)) > 32;
#endif

  /* otherwise.. */
  return false;
}


float Util::randfrac() {
  return random() / (float)RAND_MAX;
}

/* XXX, could use better source of randomness (kernel)
   on systems that support it. But we don't have any
   real need for cryptographic randomness.

   web sequence numbers are chosen randomly, now, so we
   actually do.
*/
namespace {
/* ensure that random is seeded */
struct RandomSeeder {
  RandomSeeder() {
# if defined(WIN32) || defined(__MINGW32__)
    srand((int)time(nullptr) ^ getpid());
# else
    srandom(time(0) ^ getpid());
# endif
    /* run it a bit */
    for (int i = 0; i < 256; i ++)
      (void)Util::random();
  }
};
}  // namespace

int Util::random() {
  // Run exactly once, with initialization thread safe.
  // Result is not used.
  static RandomSeeder *unused = new RandomSeeder;
  (void)unused;
# if defined(WIN32) || defined(__MINGW32__)
  return ::rand();
# else
  return ::random();
# endif
}
