
// XXX to cc-lib? ttf.h?

#ifndef _LOWERCASE_BEZIER_H
#define _LOWERCASE_BEZIER_H

#include <tuple>

std::tuple<float, float, float>
DistanceFromPointToQuadBezier(
    // The point to test
    float px, float py,
    // Bezier start point
    float sx, float sy,
    // Bezier ontrol point
    float cx, float cy,
    // Bezier end point
    float ex, float ey);

#endif
