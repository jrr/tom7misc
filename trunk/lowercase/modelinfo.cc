
#include "modelinfo.h"

#include <memory>
#include <string>
#include <cstdint>
#include <cmath>

#include "image.h"
#include "network.h"
#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;
using int64 = int64_t;

namespace {
// XXX could make sense as a standalone utility?
struct Histogram {
  void Add(float f) { values.push_back(f); }

  // Assumes width is the number of buckets you want.
  std::tuple<float, float, ImageA> MakeImage(int width, int height) const {
    ImageA img(width, height);

    float lo = std::numeric_limits<float>::infinity();
    float hi = -std::numeric_limits<float>::infinity();

    for (float v : values) {
      lo = std::min(v, lo);
      hi = std::max(v, hi);
    }
    
    const float ival = hi - lo;
    const float bucket_width = ival / width;
    const float oval = 1.0f / ival;
    if (ival <= 0) return make_tuple(lo, hi, img);

    vector<int64> count(width, 0);
    for (float v : values) {
      float f = (v - lo) * oval;
      int bucket = roundf(f * (width - 1));
      count[bucket]++;
    }

    int64 max_count = 0;
    int maxi = 0;
    for (int i = 0; i < count.size(); i++) {
      int64 c = count[i];
      if (c > max_count) {
	max_count = c;
	maxi = i;
      }
    }
    
    // Finally, fill in the image.
    for (int bucket = 0; bucket < width; bucket++) {
      double hfrac = count[bucket] / (double)max_count;
      float fh = hfrac * (height - 1);
      int h = fh;
      float fpart = fh - h;
      // don't allow zero pixels.
      // this is not accurate but I want to be able to see
      // non-empty buckets clearly
      if (h == 0 && count[bucket] > 0) {
	h = 1;
	fpart = 0.0f;
      }
      int nh = height - h;
      if (nh > 0) {
	uint8 v = roundf(fpart * 255);
	img.SetPixel(bucket, nh - 1, v);
      }
      for (int y = nh; y < height; y++) {
	CHECK(bucket < img.Width() && bucket >= 0 &&
	      y < img.Height() && y >= 0) << bucket << " " << y;
	img.SetPixel(bucket, y, 0xFF);
      }
    }

    // Label the mode.
    float center = lo + ((maxi + 0.5f) * bucket_width);
    string label = StringPrintf("%.4f", center);
    int lw = label.size() * 9;
    int x = maxi > (width / 2) ? maxi - (lw + 1) : maxi + 1;
    img.BlendText(x, 0, 0xFF, label);
    
    return make_tuple(lo, hi, img);
  }
  
  vector<float> values;
};
}  // namespace

ImageRGBA ModelInfo(const Network &net, int width, int height) {
  static constexpr int MARGIN = 4;  
  // Get histogram of weights per layer.
  // Only layers with inputs (i.e. not the input layer) have weights.
  const int num_histos = net.num_layers;

  const int HISTOW = width / 2 - MARGIN;
  const int HISTOH = height / num_histos;
  
  ImageRGBA img{width, height};
  img.Clear32(0x000000FF);
  
  for (int layer = 0; layer < net.num_layers; layer++) {
    Histogram bias_histo;
    for (float f : net.layers[layer].biases) bias_histo.Add(f);

    Histogram weight_histo;
    for (float f : net.layers[layer].weights) weight_histo.Add(f);    
    
    auto DrawHisto = [&img](const Histogram &histo, int x, int y, int w, int h,
			    const string &label) {
	const int hmargin = 10;
	const auto [lo, hi, himg] = histo.MakeImage(w, h - hmargin);
	ImageRGBA color = himg.AlphaMaskRGBA(0xFF, 0xFF, 0x00);
	img.BlendImage(x, y, color);
	const int label_y = y + (h - hmargin);
	img.BlendText32(x, label_y,
			0xFFAAAAFF,
			StringPrintf("%.9f", lo));
	const string his = StringPrintf("%.9f", hi);
	img.BlendText32(x + w - (his.size() * 9), label_y,
			0xAAFFAAFF,
			his);

	img.BlendText32(x + (w - (label.size() * 9)) / 2, label_y,
			0xFFFFAAFF,
			label);

      };

    DrawHisto(bias_histo, 0, HISTOH * layer, HISTOW, HISTOH,
	      StringPrintf("^ Bias layer %d ^", layer));

    DrawHisto(weight_histo, HISTOW + MARGIN, HISTOH * layer, HISTOW, HISTOH,
	      StringPrintf("^ Weights layer %d ^", layer));
  }

  return img;
}
