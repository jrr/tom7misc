
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
  // If tallest_bucket is, say, 0.9, the bars are stretched to go 90% of the way to
  // the top of the image (1.0 is a sensible default but can be confusing in the
  // presence of tick marks, say).
  std::tuple<float, float, ImageA> MakeImage(int width, int height,
                                             double tallest_bucket = 1.0) const {
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
      float fh = (hfrac * tallest_bucket) * (height - 1);
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
    // Align on left or right of the label so as not to run off the screen
    // (we could also try to avoid other buckets?)
    int x = maxi > (width / 2) ? maxi - (lw + 2) : maxi + 3;
    // Align with the peak, taking into account tallest_bucket.
    int y = (1.0 - tallest_bucket) * (height - 1);
    img.BlendText(x, y, 0xFF, label);

    return make_tuple(lo, hi, img);
  }

  // For example with tick=0.25, vertical lines at -0.25, 0, 0.25, 0.50, ...
  static ImageRGBA TickImage(int width, int height, float lo, float hi,
                             uint32 negative_tick_color,
                             uint32 zero_tick_color,
                             uint32 positive_tick_color,
                             float tick) {
    ImageRGBA img(width, height);
    const float ival = hi - lo;
    const float bucket_width = ival / width;

    for (int x = 0; x < width; x++) {
      const float bucket_lo = lo + x * bucket_width;
      const float bucket_hi = bucket_lo + bucket_width;
      // Does any tick edge reside in the bucket?
      // (Note there can be more than one...)

      // Floor here because we need rounding towards negative
      // infinity, not zero.
      const int tlo = floorf(bucket_lo / tick);
      const int thi = floorf(bucket_hi / tick);
      if (tlo != thi) {
        uint32 tick_color = 0;
        // tlo and thi are floor, so zero would fall in the bucket
        // [-1, 0]
        if (tlo == -1 && thi == 0) tick_color = zero_tick_color;
        else if (tlo < 0) tick_color = negative_tick_color;
        else tick_color = positive_tick_color;
        for (int y = 0; y < height; y++) {
          img.SetPixel32(x, y, tick_color);
        }
      }
    }
    return img;
  }

  // If present, samples are clipped to these bounds.
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
        constexpr int hmargin = 14;
        const auto [lo, hi, himg] = histo.MakeImage(w, h - hmargin, 0.95);
        ImageRGBA color(himg.Width(), himg.Height());

        // Always ticks at 0.1.
        const ImageRGBA timg = Histo::TickImage(w, h, lo, hi,
                                                0xFF777735,
                                                0xFFFFFF75,
                                                0x77FF7735,
                                                0.1f);
        color.BlendImage(0, 0, timg);

        // minor ticks if scale is very small?
        if (hi - lo <= 2.0f) {
          const ImageRGBA mtimg = Histo::TickImage(w, h, lo, hi,
                                                   0xFF777720,
                                                   0xFFFFFF40,
                                                   0x77FF7720,
                                                   0.025f);
          color.BlendImage(0, 0, mtimg);
        }

        color.BlendImage(0, 0, himg.AlphaMaskRGBA(0xFF, 0xFF, 0x00));

        img.BlendImage(x, y, color);
        const int label_y = y + (h - hmargin) + 1;
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

// Aesthetic mode
static inline uint32 GetWeightColor(float f, bool diagnostic_mode) {
  auto MapV = [diagnostic_mode](float f) -> uint8 {
      float ff = 0.0f;
      if (diagnostic_mode) {
        ff = sqrtf(f);
      } else {
        if (f > 0.25f) {
          ff = 1.0f;
        } else {
          ff = 4.0f * f;
          ff = sqrtf(sqrtf(ff));
        }
      }
      int v = roundf(255.0f * ff);
      if (v > 255) return 255;
      if (v < 0) return 0;
      return v;
    };

  // XXX from vacuum - configurable or remove?
  static constexpr float THRESHOLD = 0.00001f;
  if (diagnostic_mode && f == 0) {
    return 0xFFFF00FF;
  } else if (diagnostic_mode && fabs(f) < THRESHOLD) {
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
                                  bool diagnostic_mode) {
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

  const uint32 missing_weight_color =
    diagnostic_mode ? 0x000060FF : 0x000000FF;

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
      uint32 color = GetWeightColor(w, diagnostic_mode);
      img.SetPixel32(WEIGHTSX + x, WEIGHTSY + idx, color);
    }
  }

  if (diagnostic_mode) {
    for (int y = 0; y < prev_nodes; y++) {
      if (!has_reference[y]) {
        for (int x = 0; x < num_nodes; x++) {
          // XXX: Configurable?
          // Would be missing_weight_color if not detected.
          img.SetPixel32(WEIGHTSX + x, WEIGHTSY + y, 0xFFFF00FF);
        }
      }
    }
  }

  img.BlendText32(LEFT, 2, 0xCCCCCCFF,
                  StringPrintf("<--   Layer %d's nodes (%d)  ipn=%d  -->",
                               layer_idx, num_nodes, ipn));

  return img;
}
