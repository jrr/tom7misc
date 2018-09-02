
// The goals of this file format are to be compact despite
// being human-readable ASCII. The format describes two streams of
// 8-bit values; if one stream is shorter than the other, then
// trailing zeroes are assumed to make them match (a common case
// is leaving out the second player entirely). 
//
// The input consists of a series of commands.
// Whitespace is ignored, except inside tokens (series of digits or
// input bits) where it is illegal.
//
// ! and @ switch to player 1 and player 2 streams, respectively.
// (mnemonic: shift-1 and shift-2). The file must contain a stream
// for at least one player.
//
// A sequence of digits denotes a run. The next input is repeated that
// many times. A run of 0 is illegal. The comma character is equivalent
// to a run of 1.
//
// The following characters are used for the 8 input bits:
// rldutcba. (t) is for sTart and (c) is for seleCt. They should
// appear in that order. To prevent the input string from being empty,
// use _ to mean no inputs.
//
// The input bits can be preceded by + or - to make the code relative
// to the previous input. For example, ,ur2+a is the same as
// ur,ura,ura or ,ur2ura. The first input is treated as being relative
// to zero, although there's no point in using relative encoding in
// this case.
// 
// RLE (a la cc-lib) usually benefits from "anti-runs" as well. Here
// that is not useful because the variable-length encoding of inputs
// requires a delimiter, anyway, and we have many characters to spare.
//
// TODO: It's really common to alternate back and forth between two
// inputs. Wouldn't be hard to keep a two-state buffer and also make
// it possible to refer to the input that was two moves back (perhaps
// exactly or with +- kinda stuff).

#include "simplefm7.h"

#include <cstdint>
#include <vector>
#include <ctype.h>

#include "base/logging.h"
#include "../cc-lib/util.h"
#include "simplefm2.h"

typedef uint8_t uint8;

/*
  // Count of 1-bits for each 8-bit byte. Generated with this code:
  auto PopCount =
    [](uint8 x) {
      int count = 0;
      while (x) {
	count += (x & 1);
	x >>= 1;
      }
      return count;
    };
  for (int x = 0; x < 256; x++) {
    printf("%d,", PopCount(x));
  }
  printf("\n");
*/
static uint8 popcount_table[256] = {
  0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,
  3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,2,3,2,3,3,4,2,3,3,4,
  3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,
  4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,
  3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,
  6,7,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,
  4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
};

static int AppendInt(int n, string *out) {
  char s[16];
  sprintf(s, "%d", n);
  *out += &s[0];
  return strlen(s);
}

static int EncodeTo(uint8 prev, uint8 input, string *out) {
  // Every encoding has to be at least one character, so this
  // is always cheapest if the input is empty.
  if (input == 0) {
    out->push_back('_');
    return 1;
  }

  auto AddBits =
    [&out](uint8 v) {
      static const char CHARS[] = "abctudlr";
      for (int i = 7; i >= 0; i--) {
	const uint8 mask = 1 << i;
	if (v & mask) {
	  out->push_back(CHARS[i]);
	}
      }
    };
  
  // Otherwise, there are three ways to encode. With + or -,
  // we use an extra character.
  int n_abs = popcount_table[input];
  // Input must be a superset of previous.
  int n_add =
    ((input | prev) == input) ? 1 + popcount_table[input & ~prev] : 999;
  // Input must be a subset of previous.
  int n_sub =
    ((input | prev) == prev) ? 1 + popcount_table[prev & ~input] : 999;

  if (n_sub < n_add && n_sub < n_abs) {
    out->push_back('-');
    AddBits(prev & ~input);
    return n_sub;
  } else if (n_add < n_sub && n_add < n_abs) {
    out->push_back('+');
    AddBits(input & ~prev);
    return n_add;
  } else {
    // Absolute method.
    AddBits(input);
    return n_abs;
  }
}

static string Compress(vector<uint8> &inputs) {
  uint8 prev = 0;
  string out;
  int pos = 0;

  int linelength = 1;
  auto GetRun =
    [&inputs](int p) -> std::tuple<uint8, int, int> {
      CHECK(p < inputs.size());
      uint8 v = inputs[p];
      int len = 0;
      while (p < inputs.size() && inputs[p] == v) {
	p++;
	len++;
      }
      
      return {v, p, len};
  };
  
  while (pos < inputs.size()) {
    uint8 input;
    int len;
    std::tie(input, pos, len) = GetRun(pos);
    CHECK(len > 0) << "Impossible";
    if (len == 1) {
      out.push_back(',');
      linelength++;
    } else {
      linelength += AppendInt(len, &out);
    }
    linelength += EncodeTo(prev, input, &out);
    prev = input;

    if (linelength > 75) {
      out.push_back('\n');
      linelength = 0;
    }
    
  }
  return out;
}

static void SplitInputs(const vector<pair<uint8, uint8>> &inputs,
			vector<uint8> *p1, vector<uint8> *p2) {
  p1->clear();
  p2->clear();
  p1->reserve(inputs.size());
  p2->reserve(inputs.size());

  for (const pair<uint8, uint8> p : inputs) {
    p1->push_back(p.first);
    p2->push_back(p.second);
  }

  // PERF: can strip zero inputs off the end of one of them.
}

void SimpleFM7::WriteInputs2P(const string &outputfile,
			      const vector<pair<uint8, uint8>> &inputs) {
  vector<uint8> p1, p2;
  SplitInputs(inputs, &p1, &p2);
  string res = (string)"!" + Compress(p1) + "\n@" + Compress(p2) + "\n";
  CHECK(Util::WriteFile(outputfile, res)) << outputfile;
}

void SimpleFM7::WriteInputs(const string &outputfile,
			    const vector<uint8> &inputs) {
  WriteInputs2P(outputfile, SimpleFM2::ExpandTo2P(inputs));
}

vector<pair<uint8, uint8>> SimpleFM7::ReadInputs2P(const string &filename) {
  string contents = Util::ReadFile(filename);
  CHECK(!contents.empty()) << filename;

  vector<uint8> p1, p2;
  vector<uint8> *cur = nullptr;

  int pos = 0;
  enum Token {
    T_EOF,
    T_INPUTS,
    T_STREAM,
  };
  auto ReadToken =
    [&filename, &contents, &pos](uint8 prev, uint8 *inputs, int *n) {
      // 0 byte should not be in files, or anyway is treated as EOF.
      while (pos < contents.size()) {

	// With pos at the beginning of a string like +rua, return
	// the 8-bit input that it indicates.
	auto GetInputs =
	  [&contents, &pos, prev]() -> uint8 {
	    if (pos < contents.size()) {
	      const char c = contents[pos];
	      if (c == '_') {
		pos++;
		return 0;
	      }
	      // controls relative mode; optional.
	      if (c == '+' || c == '-') pos++;

	      uint8 mask = 0;
	      while (pos < contents.size()) {
		const char d = contents[pos];
		switch (d) {
		case 'r': mask |= (1 << 7); pos++; continue;
		case 'l': mask |= (1 << 6); pos++; continue;
		case 'd': mask |= (1 << 5); pos++; continue;
		case 'u': mask |= (1 << 4); pos++; continue;
		case 't': mask |= (1 << 3); pos++; continue;
		case 'c': mask |= (1 << 2); pos++; continue;
		case 'b': mask |= (1 << 1); pos++; continue;
		case 'a': mask |= (1 << 0); pos++; continue;
		}
		// On any other character (or EOF), done.
		break;
	      }
	      
	      if (c == '+') return prev | mask;
	      else if (c == '-') return prev & ~mask;
	      else return mask;

	    } else {
	      // EOF; just treat this as zero.
	      return 0;
	    }
	  };

	char c = contents[pos];
	switch (c) {
	case '!':
	  *n = 1;
	  pos++;
	  return T_STREAM;
	  break;
	case '@':
	  *n = 2;
	  pos++;
	  return T_STREAM;
	  break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  *n = 0;
	  while (pos < contents.size() &&
		 isdigit(contents[pos])) {
	    *n = (*n * 10) + (contents[pos] - '0');
	    pos++;
	  }
	  *inputs = GetInputs();
	  return T_INPUTS;
	case ',':
	  *n = 1;
	  pos++;
	  *inputs = GetInputs();
	  return T_INPUTS;
	case ' ':
	case '\n':
	case '\r':
	case '\t':
	  pos++;
	  continue;
	default:
	  CHECK(false) << "Bad character " << c << " in "
		       << filename << " @ " << pos;
	}
      }

      return T_EOF;
    };


  uint8 prev = 0;
  for (;;) {
    uint8 inputs = 0;
    int n = 0;
    const Token t = ReadToken(prev, &inputs, &n);
    switch (t) {
    case T_EOF:
      goto done;
    case T_STREAM:
      if (n == 1) cur = &p1;
      else if (n == 2) cur = &p2;
      else {
	CHECK(false) << "Bug: Selected stream " << n;
      }
      prev = 0;
      break;
    case T_INPUTS:
      CHECK(cur != nullptr) << filename << " invalid; must select stream "
	"with ! or @ before supplying inputs.";
      cur->reserve(cur->size() + n);
      while (n--) cur->push_back(inputs);
      prev = inputs;
      break;
    }
  }
 done:;
  
  const int length = (int)max(p1.size(), p2.size());
  vector<pair<uint8, uint8>> together;
  together.reserve(length);
  for (int i = 0; i < length; i++) {
    together.emplace_back((i < p1.size()) ? p1[i] : 0,
			  (i < p2.size()) ? p2[i] : 0);
  }
  return together;
}
