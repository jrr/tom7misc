/* Simplified FM2 reader. Only supports one gamepad.
   Assumes that the movie starts with hard power-on
   in the first frame. Ignores everything else. */

#ifndef __SIMPLEFM2_H
#define __SIMPLEFM2_H

#include <vector>
#include <string>
#include <utility>

#include "types.h"

#define INPUT_R (1<<7)
#define INPUT_L (1<<6)
#define INPUT_D (1<<5)
#define INPUT_U (1<<4)
#define INPUT_T (1<<3)
#define INPUT_S (1<<2)
#define INPUT_B (1<<1)
#define INPUT_A (1   )

struct SimpleFM2 {
  using string = std::string;
  template<class T> using vector = std::vector<T>;
  template<class L, class R> using pair = std::pair<L, R>;
  
  static vector<uint8> ReadInputs(const string &filename);
  static vector<pair<uint8, uint8>> ReadInputs2P(const string &filename);
  static vector<pair<uint8, uint8>> ReadInputsEx(
      const string &filename,
      vector<pair<int, string>> *subtitles);
  
  static void WriteInputs(const string &outputfile,
                          const string &romfilename,
                          const string &romchecksum,
                          const vector<uint8> &inputs);

  static void WriteInputs2P(const string &outputfile,
                            const string &romfilename,
                            const string &romchecksum,
                            const vector<pair<uint8, uint8>> &inputs);

  // Subtitles are given as a pair (frame num, string).
  static void WriteInputsWithSubtitles(
      const string &outputfile,
      const string &romfilename,
      const string &romchecksum,
      const vector<uint8> &inputs,
      const vector<pair<int, string>> &subtitles);

  static void WriteInputsWithSubtitles2P(
      const string &outputfile,
      const string &romfilename,
      const string &romchecksum,
      const vector<pair<uint8, uint8>> &inputs,
      const vector<pair<int, string>> &subtitles);

  // Convert a dense array of subtitles (one for each frame)
  // into a sparse representation suitable for the above.
  // Handles repeated subtitles efficiently.
  static vector<pair<int, string>> MakeSparseSubtitles(
      const vector<string> &dense_subtitles);

  static vector<pair<uint8, uint8>> ExpandTo2P(const vector<uint8> &inputs);
  
  static string InputToString(uint8 input);
  static string InputToColorString(uint8 input);
};

#endif
