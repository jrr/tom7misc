
#ifndef _LOWERCASE_MODELINFO_H
#define _LOWERCASE_MODELINFO_H

#include <optional>
#include <cstdint>

#include "image.h"
#include "network.h"

struct ModelInfo {
  // Generate an image with histograms of biases and weights for each
  // layer. Used in training UI or to try to diagnose stuff like runaway
  // gradients.
  //
  // If bounds are present, values are clipped. This can be helpful for
  // inspecting the many weights that are very close to zero.
  static ImageRGBA Histogram(
      const Network &net, int width, int height,
      std::optional<float> weight_bound_low = std::nullopt,
      std::optional<float> weight_bound_high = std::nullopt,
      std::optional<float> bias_bound_low = std::nullopt,
      std::optional<float> bias_bound_high = std::nullopt);

  // Generate an image of the individual weights on a single layer. The weights
  // are plotted in 2D, as though dense. The number of indices on
  // the layer and its previous layer determine the dimensions.
  // layer_idx is an index into net's Layers (0 is the first hidden layer).
  static ImageRGBA LayerWeights(
      const Network &net, int layer_idx,
      // In diagnostic mode, draw unreferenced cells, small nonzero
      // weights, and missing (sparse) inputs distinctly; it's easier
      // to see structural issues this way. Otherwise, draw the matrix
      // as though dense, and boost the visibility of small weights
      // (sqrt(sqrt(f))); it's easier to see patterns in weight values
      // this way and it's nicer on the eye.
      bool diagnostic_mode = false);
  
};
  
#endif
