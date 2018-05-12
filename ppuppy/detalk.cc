// Deconvert a compiled talk into javascript/PNG files for local
// or emergency purposes.

#include <string>
#include "../cc-lib/image.h"
#include "ppuppy.h"
#include "screen.h"
#include "convert.h"
#include "base/stringprintf.h"
#include "util.h"
#include "deconversion.h"
#include "talk.h"

static void Save(const ImageRGBA &img,
		 const string &outfile) {
  img.Save(outfile);
  fprintf(stderr, "Wrote %s\n", outfile.c_str());
}

int main(int argc, char **argv) {
  const char *USAGE = "./detalk.exe input.ctalk input.screens outdir\n";
  CHECK(argc == 4) << USAGE;

  CompiledTalk ctalk(argv[1], argv[2]);

  // Save all PNG images.
  for (int i = 0; i < ctalk.NumScreens(); i++) {
    Screen *s = ctalk.GetScreen(i);
    ImageRGBA img = Deconvert(*s);
    Save(img, StringPrintf("%s/%d.png", argv[3], i));
  }

  string json = "var slides = [\n";
  for (int i = 0; i < ctalk.NumSlides(); i++) {
    CompiledTalk::Slide *s = ctalk.GetSlide(i);
    json += "  [";
    for (auto p : s->screens)
      json += StringPrintf("{i: %d, d: %d}, ", p.first, p.second);
    json += "],\n";
  }
  json += "];\n";

  Util::WriteFile(StringPrintf("%s/slides.js", argv[3]), json);

  Util::WriteFile(StringPrintf("%s/talk.html", argv[3]),
		  Util::ReadFile("talk.html"));
  return 0;
}
