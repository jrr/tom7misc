
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
using uint32 = uint32_t;
using int64 = int64_t;

namespace {
// XXX could make sense as a standalone utility?
struct Histo {
  void Add(float f) {
    if (bound_low.has_value() && f < bound_low.value())
      f = bound_low.value();
    if (bound_high.has_value() && f > bound_high.value())
      f = bound_high.value();

    values.push_back(f);
  }

  Histo() {}
  Histo(optional<float> bound_low, optional<float> bound_high) :
    bound_low(bound_low), bound_high(bound_high) {}

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

  optional<float> bound_low = nullopt, bound_high = nullopt;

  vector<float> values;
};
}  // namespace

ImageRGBA ModelInfo::Histogram(
    const Network &net, int width, int height,
    std::optional<float> weight_bound_low,
    std::optional<float> weight_bound_high,
    std::optional<float> bias_bound_low,
    std::optional<float> bias_bound_high) {
  static constexpr int MARGIN = 8;
  // Get histogram of weights per layer.
  // Only layers with inputs (i.e. not the input layer) have weights.
  const int num_histos = net.num_layers;

  const int HISTOW = width / 2 - MARGIN;
  const int HISTOH = height / num_histos;

  ImageRGBA img{width, height};
  img.Clear32(0x000000FF);

  for (int layer = 0; layer < net.num_layers; layer++) {
    Histo bias_histo{bias_bound_low, bias_bound_high};
    for (float f : net.layers[layer].biases) bias_histo.Add(f);

    Histo weight_histo{weight_bound_low, weight_bound_high};
    for (float f : net.layers[layer].weights) weight_histo.Add(f);

    auto DrawHisto = [&img](const Histo &histo, int x, int y, int w, int h,
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

static inline uint32 GetWeightColor(float f) {
  auto MapV = [](float f) -> uint8 {
      float ff = sqrtf(f);
      int v = roundf(255.0f * ff);
      if (v > 255) return 255;
      if (v < 0) return 0;
      return v;
    };

  // XXX from vacuum - configurable or remove?
  static constexpr float THRESHOLD = 0.00001f;  
  if (f == 0) {
    return 0xFFFF00FF;
  } else if (fabs(f) < THRESHOLD) {
    return 0xFF00FFFF;
  } else if (f > 0) {
    uint32 v = MapV(f);
    return (v << 16) | 0xFF;
  } else {
    uint32 v = MapV(-f);
    return (v << 24) | 0xFF;
  }
}

ImageRGBA ModelInfo::LayerWeights(const Network &net, int layer_idx,
				  uint32 missing_weight_color) {
  CHECK(layer_idx >= 0 && layer_idx < net.layers.size());
  const Network::Layer &layer = net.layers[layer_idx];
  const int prev_nodes = net.num_nodes[layer_idx];
  const int num_nodes = net.num_nodes[layer_idx + 1];
  const int ipn = layer.indices_per_node;
  
  //     <--- this layer's nodes --->
  // ^
  // |
  // weights
  // |
  // |
  // v
  constexpr int TOP = 12;
  constexpr int LEFT = 12;
  constexpr int RIGHT = 4;
  constexpr int BOTTOM = 4;
  ImageRGBA img(LEFT + RIGHT + num_nodes,
		TOP + BOTTOM + prev_nodes);

  constexpr int WEIGHTSX = LEFT;
  constexpr int WEIGHTSY = TOP;
  img.Clear32(0x000000FF);
  img.BlendRect32(WEIGHTSX, WEIGHTSY, num_nodes, prev_nodes,
		  missing_weight_color);

  vector<bool> has_reference(prev_nodes, false);
  
  for (int x = 0; x < num_nodes; x++) {
    const int start = x * ipn;
    for (int i = 0; i < ipn; i++) {
      uint32 idx = layer.indices[start + i];
      float w = layer.weights[start + i];

      CHECK(idx < prev_nodes);
      has_reference[idx] = true;
      uint32 color = GetWeightColor(w);
      img.SetPixel32(WEIGHTSX + x, WEIGHTSY + idx, color);
    }
  }

  for (int y = 0; y < prev_nodes; y++) {
    if (!has_reference[y]) {
      for (int x = 0; x < num_nodes; x++) {
	// XXX: Configurable?
	// Would be missing_weight_color if not detected.
	img.SetPixel32(WEIGHTSX + x, WEIGHTSY + y, 0xFFFF00FF);
      }
    }
  }
  
  img.BlendText32(LEFT, 2, 0xCCCCCCFF,
		  StringPrintf("<--   Layer %d's nodes (%d)  ipn=%d  -->",
			       layer_idx, num_nodes, ipn));
  
  return img;
}
