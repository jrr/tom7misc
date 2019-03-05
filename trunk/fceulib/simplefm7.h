/* "FM7" format reader and writer.
   Like SimpleFM2, assumes a sequence of inputs from hard power-on;
   no metadata. No subtitle support. The only advantage over FM2 is
   that the files are much, much smaller.

   The runtime representation is compatible with SimpleFM2, so it's
   easy to use the two together.
 */

#ifndef __SIMPLEFM7_H
#define __SIMPLEFM7_H

#include <vector>
#include <string>
#include <utility>

#include "types.h"

using namespace std;

struct SimpleFM7 {
  static vector<pair<uint8, uint8>> ReadInputs2P(const string &filename);
  // Same, but discards the second player.
  static vector<uint8> ReadInputs(const string &filename);

  // Same, but from a string.
  static vector<pair<uint8, uint8>> ParseString2P(const string &contents);
  static vector<uint8> ParseString(const string &contents);
  
  static void WriteInputs2P(const string &outputfile,
                            const vector<pair<uint8, uint8>> &inputs);
  // Second player always 0.
  static void WriteInputs(const string &outputfile,
                          const vector<uint8> &inputs);
};

#endif
