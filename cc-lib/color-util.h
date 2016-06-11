#ifndef __COLORUTIL_H
#define __COLORUTIL_H

struct ColorUtil {
  static void HSVToRGB(float hue, float saturation, float value,
		       float *r, float *g, float *b);

};

#endif
