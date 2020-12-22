
#ifndef _LOWERCASE_FONT_PROBLEM_H
#define _LOWERCASE_FONT_PROBLEM_H

#include <string>
#include <vector>

class Network;
class TTF;

// Runs the given eval on the CPU, for a specific font.
// Generates an image to the given filename.
struct FontProblem {

  // Fill the buffer (which must be big enough) with contours.
  // Returns true if successful.
  static bool FillVector(const TTF *ttf, int codepoint,
			 const std::vector<int> &row_max_points,
			 float *buffer);
  
  // Requires that the network be the kind of the first experiment,
  // with input and output layer starting with a fixed number of
  // rows of bezier-only paths.
  static void RenderVector(const std::string &font_filename,
			   const Network &net,
			   const std::vector<int> &row_max_points,
			   const std::string &out_filename);    

};

#endif
