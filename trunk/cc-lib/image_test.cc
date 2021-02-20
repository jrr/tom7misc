
#include "image.h"

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "randutil.h"

static void TestBilinearResize() {
  ImageA in(20, 20);
  in.Clear(0);
  for (int i = 0; i < 16; i++) {
    in.SetPixel(i, i / 2, i * 15);
  }

  in.BlendText(1, 9, 0xCC, ":)");
  in.GreyscaleRGBA().Save("test-bilinear-resize-in.png");

  ImageRGBA big = in.ResizeBilinear(120, 120).GreyscaleRGBA();
  big.Save("test-bilinear-resize-out.png");
}


int main(int argc, char **argv) {
  TestBilinearResize();

  // XXX make tests for images!
  return 0;
}
