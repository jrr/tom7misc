
#ifndef _LOWERCASE_FONT_PROBLEM_H
#define _LOWERCASE_FONT_PROBLEM_H

#include <string>
#include <vector>

class Network;
class TTF;
class ArcFour;

struct FontProblem {

  // Size of e.g. the input feature vector.
  static int BufferSizeForPoints(const std::vector<int> &row_max_points);
  
  // Fill the buffer (which must be big enough) with contours.
  // Returns true if successful.
  static bool FillVector(const TTF *ttf, int codepoint,
			 const std::vector<int> &row_max_points,
			 float *buffer);

  // Runs the given eval on the CPU, for a specific font.
  // Generates an image to the given filename.
  //
  // Requires that the network be the kind of the first experiment,
  // with input and output layer starting with a fixed number of
  // rows of bezier-only paths.
  static void RenderVector(const std::string &font_filename,
			   const Network &net,
			   const std::vector<int> &row_max_points,
			   const std::string &out_filename);    

  // Code for computing the error between a predicted vector shape ("loop")
  // and the expected one.
  //
  // When we predict a loop it is of fixed size, but the expected
  // font's loop is generally smaller. The predicted loop needs to
  // follow the same sequence of vertices, but to use up its extra
  // points, it is permitted to duplicate them. (At first I was asking
  // it to always duplicate the start point at the end of the loop,
  // but this may be more rigid than we want.) Alas we cannot "just"
  // compute some geometric distance between the two loops, because we
  // also need to attribute the error to specific points (including
  // Bezier control points), including the error's derivative.
  //
  // XXX docs are out of date
  // Takes an expected loop and actual loop as a series of points 
  // (for this code, we can just think of the edges as straight lines).
  // Finds a mapping from each expected point to some actual point,
  // such that:
  //   - the expected points appear in strictly increasing order
  //       (modulo the loop)
  //   - the error is minimized:
  //       - for each actual point in the domain of the mapping,
  //         its Euclidean distance to the expected point
  //       - also, all the unmapped points between it and the
  //         previously mapped point.
  //
  // The output is a vector the same length as expected, which gives
  // the index of the mapped point in actual.
  // Note: Expensive!
  using Point = std::pair<float, float>;
  // An assignment can be equivalently specified as a start point
  // (index into a) and then the number of expected points consumed
  // by each point a (>= 1, summing to |expected|).
  struct LoopAssignment {
    explicit LoopAssignment(int size) : groups(size, 1) {}
    // index into assignments
    int point0 = 0;
    // size |expected|. number of actual points that are
    // grouped into the expected point.
    std::vector<int> groups;
  };
  LoopAssignment BestLoopAssignment(ArcFour *rc,
				    const std::vector<Point> &expected,
				    const std::vector<Point> &actual);
};

#endif
