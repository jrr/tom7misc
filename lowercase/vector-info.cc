
#include <vector>
#include <string>

#include "loadfonts.h"

using namespace std;

int main(int argc, char **argv) {

  VectorLoadFonts loadfonts(
      []() { return false; },
      std::vector<int>{1000, 1000, 1000},
      24,
      1'000'000);

  loadfonts.Sync();

  string letters =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  
  FILE *f = fopen("info.tsv", "wb");

  // we could use a ragged representation here where the
  // 2nd and 3rd contour have fewer points allocated...
  
  fprintf(f, "# contours\tmax p 0\tmax p 1\t max p 2\n");
  for (TTF *ttf : loadfonts.fonts) {
    int mc = 0;
    // If we sort the contours by length, max of the largest,
    // second largest, third largest.
    int mp0 = 0, mp1 = 0, mp2 = 0;
    for (int idx = 0; idx < letters.size(); idx++) {
      int c = letters[idx];
      
      std::vector<TTF::Contour> contours =
	// XXX just to ensure that expectations are met
	TTF::NormalizeOrder(
	    ttf->GetContours(c),
	    0.0f, 0.0f);
      mc = std::max(mc, (int)contours.size());
      std::vector<int> path_lengths;
      for (const TTF::Contour &c : contours) {
	path_lengths.push_back(c.paths.size());
      }

      std::sort(path_lengths.begin(), path_lengths.end(),
		[](int a, int b) { return b < a; });

      if (path_lengths.size() > 0)
	mp0 = std::max(mp0, path_lengths[0]);
      if (path_lengths.size() > 1)
	mp1 = std::max(mp1, path_lengths[1]);
      if (path_lengths.size() > 2)
	mp2 = std::max(mp2, path_lengths[2]);

    }

    fprintf(f, "%d\t%d\t%d\t%d\n", mc, mp0, mp1, mp2);
  }
  fclose(f);
  
  return 0;
}
