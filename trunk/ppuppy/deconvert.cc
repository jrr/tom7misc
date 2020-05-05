// Round-trip converts a graphic into Screen format, then back to RGBA
// PNG, for debugging.

#include <string>
#include "../cc-lib/image.h"
#include "ppuppy.h"
#include "screen.h"
#include "convert.h"
#include "base/stringprintf.h"
#include "util.h"
#include "deconversion.h"

static void Save(const ImageRGBA &img,
		 const string &outfile) {
  img.Save(outfile);
  fprintf(stderr, "Wrote %s\n", outfile.c_str());
}

int main(int argc, char **argv) {
  // fprintf(stderr, "Byte size of screen: %d\n", (int)sizeof(Screen));
  const char *USAGE = "./deconvert.exe [-multi] input.ext out.png\n";
  CHECK(argc >= 2) << USAGE;
  bool multi = false;

  int argi = 1;
  if (0 == strcmp(argv[argi], "-multi")) {
    multi = true;
    argi++;
  }

  CHECK(argi < argc) << USAGE;
  string infile = argv[argi++];
  CHECK(argi < argc) << USAGE;
  string base = argv[argi++];
  
  CHECK(Util::EndsWith(base, ".png")) << base;
  base.resize(base.size() - 4);

  if (multi) {
    vector<Screen> screens = MultiScreenFromFile(infile);
    for (int i = 0; i < screens.size(); i++) {
      const Screen &screen = screens[i];
      const string outfile = StringPrintf("%s-%d.png", base.c_str(), i);
      ImageRGBA img = Deconvert(screen);
      Save(img, outfile);
    }
  } else {
    Screen screen = ScreenFromFile(infile);
    ImageRGBA img = Deconvert(screen);
    // ImageRGBA img = Swatches();
    Save(img, base + ".png");
  }
  return 0;
}
