
#ifndef _LOWERCASE_MODELINFO_H
#define _LOWERCASE_MODELINFO_H

#include <optional>

#include "image.h"
#include "network.h"

// If bounds are present, values are clipped. This can be helpful for
// inspecting the many weights that are very close to zero.
ImageRGBA ModelInfo(
    const Network &net, int width, int height,
    std::optional<float> weight_bound_low = std::nullopt,
    std::optional<float> weight_bound_high = std::nullopt,
    std::optional<float> bias_bound_low = std::nullopt,
    std::optional<float> bias_bound_high = std::nullopt);

#endif
