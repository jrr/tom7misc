
#ifndef _LOWERCASE_FONT_PROBLEM_H
#define _LOWERCASE_FONT_PROBLEM_H

#include <string>
#include <vector>
#include <algorithm>

#include "ttf.h"

class Network;
class ArcFour;

struct FontProblem {

#if 0
  // Config for generating SDFs. See ttf.h.
  struct SDFConfig {
    int sdf_size = 64;
    int pad_top = 4;
    int pad_bot = 18;
    int pad_left = 18;
    uint8_t onedge_value = 200u;
    float falloff_per_pixel = 7.860f;
  };
#endif
  struct SDFConfig {
    int sdf_size = 36;
    int pad_top = 2;
    int pad_bot = 9;
    int pad_left = 9;
    uint8_t onedge_value = 220u;
    float falloff_per_pixel = 15.0f;
  };

  static inline uint8_t FloatByte(float f) {
    const int x = roundf(f * 255.0f);
    return std::clamp(x, 0, 255);
  }

  // Size of e.g. the input feature vector.
  static int BufferSizeForPoints(const std::vector<int> &row_max_points);

  // Fill the buffer (which must be big enough) with contours.
  // Returns true if successful.
  static bool FillVector(const TTF *ttf, int codepoint,
                         const std::vector<int> &row_max_points,
                         float *buffer);

  // The first part of FillVector. Normalizes contours to Beziers and
  // puts their start point close to 0,0. Checks that the contours
  // will fit, puts them in sorted order, and adds empty contours to
  // match row_max_points. Does not pad contours, however.
  // Returns true if successful.
  static bool GetRows(const TTF *ttf, int codepoint,
                      const std::vector<int> &row_max_points,
                      std::vector<TTF::Contour> *contours);

  static void FillExpectedVector(
      ArcFour *rc,
      // Config; both the expected and predicted must match.
      const std::vector<int> &row_max_points,
      // The actual output from the training example (e.g. GetRows above).
      const std::vector<TTF::Contour> &expected_contours,
      // The flat predicted vectors. These are used to determine the
      // best fit among equivalent representations of the expected_contours.
      const std::vector<float> &predicted,
      // Floats are written to the beginning of this vector, which
      // must be large enough.
      std::vector<float> *buffer);


  // Runs the given eval on the CPU, for a specific font.
  // Generates an image to the given filename.
  //
  // Requires that the network be the kind of the first experiment,
  // with input and output layer starting with a fixed number of
  // rows of bezier-only paths.
  static void RenderVector(const std::string &font_filename,
                           const Network &net,
                           const std::vector<int> &row_max_points,
                           const std::string &out_filename);

  // Because this generates SDFs and runs two large networks, it's
  // significantly slower than the above.
  static void RenderSDF(const std::string &font_filename,
                        const Network &make_lowercase,
                        const Network &make_uppercase,
                        const SDFConfig &config,
                        // writes several files.
                        // "-uppercase.png" etc. is added.
                        const std::string &base_out_filename);

  // For a buffer beginning with an SDF of the appropriate size
  // (as floats 0-1), build the SDF image.
  static ImageA SDFGetImage(const SDFConfig &config,
                            const std::vector<float> &buffer);

  // Compute an SDF from a bitmap image. The image should be
  // square and significantly larger than the SDF size, or
  // else the results will probably be quite bad. Not efficient.
  //
  // The padding in the SDFConfig has no effect.
  // Input image is treated as a 1-bit bitmap (zero/nonzero).
  static ImageA SDFFromBitmap(const SDFConfig &config,
                              const ImageA &img);

  // A tiny bitmap, like a letter. Intended for 8x8 bitmaps,
  // but can be used for any size <= 8x8.
  struct Image8x8 {
    inline void SetPixel(int x, int y, bool v) {
      int b = y * 8 + x;
      if (v) {
        bits |= ((uint64_t)1 << b);
      } else {
        bits &= ~((uint64_t)1 << b);
      }
    }
    inline bool GetPixel(int x, int y) {
      int b = y * 8 + x;
      return (bits >> b) & 1;
    }
    uint64_t bits = 0;
  };

  // Specific to the 36x36 bitmap problem I mostly worked with;
  // tuned for speed. The 8x8 bitmap is placed at 4,6, which
  // empirically seems to be the "right" place for it.
  static ImageA SDF36From8x8(Image8x8 bits);

  // Render tresholded image at high resolution (each dimension scaled
  // by scale), then downsample to get an anti-aliased image. Output
  // image is the same size as the input sdf, so typically this follows
  // upscaling the SDF with bilinear resampling.
  static ImageA SDFThresholdAA(uint8_t onedge_value,
                               const ImageA &sdf,
                               int scale = 2);

  // Run the model on a single SDF. Returns the resulting SDF and
  // the letter predictors (nominally in [0,1]).
  static std::pair<ImageA, std::array<float, 26>>
  RunSDFModel(const Network &net,
              const SDFConfig &config,
              const ImageA &sdf_input);


  // Threshold the sdf at multiple values and emit an anti-aliased,
  // layered image. The layers should have increasing threshold
  // values (first field) and only the RGB channels of the color
  // (second second) are used. Typical to include config.onedge_value
  // as one of them!
  // scale is a oversampling quality param.
  static ImageRGBA ThresholdImageMulti(
      const ImageA &sdf,
      const std::vector<std::pair<uint8_t, uint32_t>> &layers,
      int scale = 4);


  struct GenResult {
    GenResult(int w, int h) :
      input(w, h),
      low(w, h),
      low_up(w, h),
      up(w, h),
      up_low(w, h) {}

    ImageRGBA input;
    std::array<float, 26> low_pred;
    std::array<float, 26> up_pred;
    ImageRGBA low;
    ImageRGBA low_up;
    ImageRGBA up;
    ImageRGBA up_low;
  };

  // Generate 5 images (scaled according to the parameter): The
  // input SDF, the predicted uppercase and lowercase,
  // uppercase(lowercase) and lowercase(uppercase).
  static GenResult GenImages(const FontProblem::SDFConfig &config,
                             const Network &make_lowercase,
                             const Network &make_uppercase,
                             const ImageA &sdf,
                             int scale);

  // Code for computing the error between a predicted vector shape ("loop")
  // and the expected one.
  //
  // When we predict a loop it is of fixed size, but the expected
  // font's loop is generally smaller. The predicted loop needs to
  // follow the same sequence of vertices, but to use up its extra
  // points, it is permitted to duplicate them. (At first I was asking
  // it to always duplicate the start point at the end of the loop,
  // but this may be more rigid than we want.) Alas we cannot "just"
  // compute some geometric distance between the two loops, because we
  // also need to attribute the error to specific points (including
  // Bezier control points), including the error's derivative.
  //
  // XXX docs are out of date
  // Takes an expected loop and actual loop as a series of points
  // (for this code, we can just think of the edges as straight lines).
  // Finds a mapping from each expected point to some actual point,
  // such that:
  //   - the expected points appear in strictly increasing order
  //       (modulo the loop)
  //   - the error is minimized:
  //       - for each actual point in the domain of the mapping,
  //         its Euclidean distance to the expected point
  //       - also, all the unmapped points between it and the
  //         previously mapped point.
  //
  // The output is a vector the same length as expected, which gives
  // the index of the mapped point in actual.
  // Note: Expensive!
  using Point = std::pair<float, float>;
  // An assignment can be equivalently specified as a start point
  // (index into a) and then the number of expected points consumed
  // by each point a (>= 1, summing to |expected|).
  struct LoopAssignment {
    explicit LoopAssignment(int size) : groups(size, 1) {}
    // index into assignments
    int point0 = 0;
    // size |expected|. number of actual points that are
    // grouped into the expected point.
    std::vector<int> groups;
  };
  static LoopAssignment BestLoopAssignment(ArcFour *rc,
                                           const std::vector<Point> &expected,
                                           const std::vector<Point> &actual);
};

#endif
