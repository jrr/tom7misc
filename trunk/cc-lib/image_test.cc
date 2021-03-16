
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

// XXX this just tests clipping; actually test the sampling!
static void TestSampleBilinear() {
  ImageF in(5, 5);
  in.Clear(1.0f);

  {
    // This sample is well outside the image, so it should
    // return the third arg.
    float out = in.SampleBilinear(10.0, 10.0, 0.25);
    CHECK(fabs(out - 0.25) < 0.00001);
  }
}

static void TestEq() {
  ImageA one(10, 20);
  one.Clear(0x7F);
  ImageA two = one;
  CHECK(two == one);
  two.SetPixel(3, 9, 0x11);
  CHECK(!(two == one));
}

int main(int argc, char **argv) {
  TestBilinearResize();
  TestSampleBilinear();
  TestEq();
  
  // XXX make tests for images!

  printf("OK\n");
  return 0;
}
