#ifndef __COLORUTIL_H
#define __COLORUTIL_H

struct ColorUtil {
  static void HSVToRGB(float hue, float saturation, float value,
		       float *r, float *g, float *b);
  // Here RGB channels are nominally in [0, 1].
  // L is nominally [0,100].
  static void RGBToLAB(float r, float g, float b,
		       float *ll, float *aa, float *bb);

  // CIE1994 distance between sample color Lab2 and reference Lab1.
  // Careful: This may not even be symmetric!
  static float DeltaE(float l1, float a1, float b1,
		      float l2, float a2, float b2);
};

#endif
