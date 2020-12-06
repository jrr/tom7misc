// Mark fonts whose characters for a-z seem to be simple
// transformations (x = ax + b, y = cx + d) of A-Z.

#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>
#include <unordered_set>
#include <mutex>
#include <unordered_map>

#include "util.h"
#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "city/city.h"

#include "ttf.h"
#include "stb_truetype.h"

using namespace std;

using uint8 = uint8_t;
using int64 = int64_t;

enum Result {
  NOT_STRUCTURAL,
  NO_TRANSFORM,
  SAME_CASE,
};
  

int main(int argc, char **argv) {

  vector<string> all_filenames = Util::ReadFileToLines("all_fonts.txt");

  vector<Result> results =
  ParallelMap(all_filenames,
	      [&](const string &filename) {
		TTF ttf{filename};

		// Iteratively try to solve for a transformation.
		// Probably this should be done with some off-the-shelf solver!

		// Pair of uppercase/lowercase contours.
		// We've already verified that each shape is structurally
		// the same, so these are just x,y coordinates that we expect
		// to have some linear relationship (if the hypothesis holds).
		// The two vectors have the same length.
		struct Pair {
		  vector<pair<float, float>> ucase, lcase;
		};

		vector<Pair> pairs;
		
		for (char upper = 'A'; upper <= 'Z'; upper++) {
		  const char lower = upper | 32;
		  vector<TTF::Contour> ucase = ttf.GetContours(upper);
		  vector<TTF::Contour> lcase = ttf.GetContours(lower);

		  // Must have the same number of contours.
		  if (ucase.size() != lcase.size())
		    return NOT_STRUCTURAL;

		  Pair p;		  
		  // Each contour must have the same number of paths
		  // (and be in the same order).
		  for (int i = 0; i < ucase.size(); i++) {
		    const TTF::Contour &ucont = ucase[i];
		    const TTF::Contour &lcont = lcase[i];
		    p.ucase.emplace_back(ucont.startx, ucont.starty);
		    p.lcase.emplace_back(lcont.startx, lcont.starty);
		    
		    const auto &up = ucont.paths;
		    const auto &lp = lcont.paths;
		    if (up.size() != lp.size())
		      return NOT_STRUCTURAL;
		    
		    for (int j = 0; j < up.size(); j++) {
		      // And same point type.
		      if (up[j].type != lp[j].type) {
			return NOT_STRUCTURAL;
		      } else {
			switch (up[j].type) {
			case TTF::PathType::LINE:
			  p.ucase.emplace_back(up[j].x, up[j].y);
			  p.lcase.emplace_back(lp[j].x, lp[j].y);
			  break;
			case TTF::PathType::BEZIER:
			  p.ucase.emplace_back(up[j].x, up[j].y);
			  p.ucase.emplace_back(up[j].cx, up[j].cy);
			  p.lcase.emplace_back(lp[j].x, lp[j].y);
			  p.lcase.emplace_back(lp[j].cx, lp[j].cy);
			  break;
			}
		      }
		    }
		  }

		  // There have to be some points??
		  if (p.ucase.empty())
		    return NOT_STRUCTURAL;
		  CHECK(p.ucase.size() == p.lcase.size());
		  
		  pairs.push_back(std::move(p));
		}
		
		// All characters are structurally the same. Can
		// we find a linear transformation now?

		// XXX do it
		
		return SAME_CASE;
	      }, 12);
  
  return 0;
}



