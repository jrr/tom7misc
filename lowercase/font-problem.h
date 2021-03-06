
#ifndef _LOWERCASE_FONT_PROBLEM_H
#define _LOWERCASE_FONT_PROBLEM_H

#include <string>
#include <vector>
#include <algorithm>
#include <bit>

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
    inline bool GetPixel(int x, int y) const {
      int b = y * 8 + x;
      return (bits >> b) & 1;
    }
    int Edges() const {
      int hedges = 0, vedges = 0;
      for (int y = 0; y < 8; y++) {
        if (GetPixel(0, y)) hedges++;
        if (GetPixel(7, y)) hedges++;
        for (int x = 0; x < 7; x++)
          if (GetPixel(x, y) != GetPixel(x + 1, y))
            hedges++;
      }
      for (int x = 0; x < 8; x++) {
        if (GetPixel(x, 0)) vedges++;
        if (GetPixel(x, 7)) vedges++;
        for (int y = 0; y < 7; y++)
          if (GetPixel(x, y) != GetPixel(x, y + 1))
            vedges++;
      }
      return hedges + vedges;
    }
    int PixelsOn() const {
      // XXX in c++20, with #include <bit>,
      // return std::popcount(bits);
      return __builtin_popcountll(bits);
    }
    uint64_t bits = 0;
  };

  // Specific to the 36x36 bitmap problem I mostly worked with;
  // tuned for speed. The 8x8 bitmap is placed at 4,6, which
  // empirically seems to be the "right" place for it.
  //
  // Note that these SDFs agree with SDFFromBitmap but are not
  // really right (they are too "low res") because that routine
  // assumes the bitmap is at least as big as the output sdf.
  static ImageA SDF36From8x8(Image8x8 bits);

  // Deprecated!
  static ImageA SDFThresholdAA(uint8_t onedge_value,
                               const ImageA &sdf,
                               int scale);

  // Prefer this new version. threshold is in [0, 1].
  // width and height are the size of the output image. quality
  // controls the amount of oversampling (this many pixels squared,
  // per output pixel) for anti-aliasing.
  // (TODO: Perhaps should take ImageF?)
  static ImageA SDFThresholdAAFloat(float onedge_value,
                                    const ImageA &sdf,
                                    int width,
                                    int height,
                                    int quality = 2,
                                    float gamma = 1.0f);


  // Run the model on a single SDF. Returns the resulting SDF and
  // the letter predictors (nominally in [0,1]).
  static std::pair<ImageA, std::array<float, 26>>
  RunSDFModel(const Network &net,
              const SDFConfig &config,
              const ImageA &sdf_input);


  // Delete nodes from the final layer such that it predicts
  // only the 26 letter predictors. This makes the network faster
  // to run for that purpose, which speeds up hallucinate.exe.
  static Network *MakePredOnlyNetwork(const SDFConfig &config,
                                      const Network &net);
  // Must use a network generated by the previous.
  // Returns the vector directly to avoid a copy.
  static std::vector<float>
  RunSDFModelPredOnly(const Network &net,
                      const SDFConfig &config,
                      const ImageA &sdf_input);

  // Threshold the sdf at multiple values and emit an anti-aliased,
  // layered image. The layers should have increasing threshold values
  // (first field, in [0,1]) and only the RGB channels of the color
  // (second) are used. Typical to include config.onedge_value/255.0f
  // as one of them!
  static ImageRGBA ThresholdImageMulti(
      const ImageA &sdf,
      const std::vector<std::pair<float, uint32_t>> &layers,
      int out_width, int out_height,
      int quality = 4,
      float gamma = 1.0f);

  // SDFs resulting from iterating the model.
  struct Gen5Result {
    SDFConfig config;
    ImageA input;
    std::array<float, 26> low_pred;
    std::array<float, 26> up_pred;

    ImageA low;
    ImageA low_up;
    ImageA up;
    ImageA up_low;
  };

  // Returns 5 sdfs and the letter predictions: A copy of the input
  // SDF, the predicted uppercase and lowercase, uppercase(lowercase)
  // and lowercase(uppercase).
  static Gen5Result Gen5(const SDFConfig &config,
                         const Network &make_lowercase,
                         const Network &make_uppercase,
                         const ImageA &sdf);

  struct Gen5ImagesResult {
    Gen5ImagesResult(int w, int h) :
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

  // Using the result above, generate scaled images and thresholded
  // AA images using the builtin layering, after applying the gamma
  // params. (This should be pretty fast after running the model)
  static Gen5ImagesResult Gen5Images(const Gen5Result &result,
                                     int scale,
                                     int quality = 3,
                                     float gamma_low = 1.0f,
                                     float gamma_up = 1.0f);

  // Combines the above for convenience.
  static Gen5ImagesResult Gen5Images(const SDFConfig &config,
                                     const Network &make_lowercase,
                                     const Network &make_uppercase,
                                     const ImageA &sdf,
                                     int scale,
                                     int quality = 3,
                                     float gamma_low = 1.0f,
                                     float gamma_up = 1.0f);

  // Approximate the SDF with vectors.
  // Returns {unoptimized, optimized}.
  static std::pair<std::vector<TTF::Contour>, std::vector<TTF::Contour>>
  VectorizeSDF(
      const SDFConfig &config,
      const ImageA &sdf,
      // debugging image
      ImageRGBA *islands = nullptr);

  static TTF::Contour OptimizedContour(
      const ImageF &sdf,
      const ImageA &bitmap,
      const std::vector<std::pair<float, float>> &points,
      float error_threshold = 0.05f);

  // Guess the right edge of a character. Result is in SDF coordinates
  // (i.e. in [0, 36]). Assumes that we want symmetric padding on the
  // left and right (between ascent and baseline).
  static float GuessRightEdge(
      const SDFConfig &config,
      const ImageF &sdf);

  // TODO: Rote conversion to TTF "Char" format. Basically just normalizes the
  // coordinates.
  static TTF::Char ToChar(
      const SDFConfig &config,
      const std::vector<TTF::Contour> &contours,
      float right_edge);

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
