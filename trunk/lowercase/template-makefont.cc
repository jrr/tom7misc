
#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"
#include "font-problem.h"

#include "image.h"
#include "lines.h"
#include "base/stringprintf.h"

#include "threadutil.h"

using namespace std;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;


// XXX somehow needs to be shared?
static constexpr int ROW0_MAX_PTS = 38;
static constexpr int ROW1_MAX_PTS = 14;
static constexpr int ROW2_MAX_PTS = 10;
constexpr std::initializer_list<int> row_max_points = {
  ROW0_MAX_PTS,
  ROW1_MAX_PTS,
  ROW2_MAX_PTS,
};


struct Op {
  char input_char;
  char output_char;
  Op(char i, char o) : input_char(i), output_char(o) {}
};

struct Config {
  string font_name = "Upperercase and Lowerercase";
  string filename = "upperercase.sfd";
  string copyright = "http://tom7.org/lowercase - "
    "Hand-drawn font with 'extra uppercase' and 'extra lowercase' letters, "
    "plus matching (but normal) numbers and punctuation to round out "
    "the en-US keyboard characters. Copyright (c) 2021 Tom Murphy 7. "
    "Distribute freely";
  float baseline = 0.75f;
  float extra_scale = 1.0f;
  float linegap = 0.0f;
  float blank_width = 0.66f;

  int BSIZE = 800;
  
  vector<pair<string, string>> templates = {
    {"upperercase.png",
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"},
    {"lowerercase.png",
     "abcdefghijklmnopqrstuvwxyz"},
    {"numbers.png",
     "1234567890!@#$%^&*()"},
    {"punctuation.png",
     "-=_+[]{}\\|;:'\"<>?,./`~"},
  };
  // TODO punctuation...
};

// static constexpr FontProblem::SDFConfig SDF_CONFIG = {};
// Use 2x config
static constexpr FontProblem::SDFConfig SDF_CONFIG = {
  .sdf_size = 36 * 2,
  .pad_top = 2 * 2,
  .pad_bot = 9 * 2,
  .pad_left = 9 * 2,
  .onedge_value = 220u,
  .falloff_per_pixel = 15.0f / 2.0f,
};

static ImageRGBA LoadOrDie(const string &filename) {
  std::unique_ptr<ImageRGBA> img(ImageRGBA::Load(filename));
  CHECK(img.get() != nullptr);
  // Copy
  return *img;
}

// Just making a TTF from precomputed bitmaps.
static void GenerateOne(Config cfg) {
  std::mutex out_m;

  // Chop into bitmaps to process.
  vector<pair<char, ImageRGBA>> bitmaps;
  for (const auto &[tpl_file, tpl_chars] : cfg.templates) {
    ImageRGBA tpl = LoadOrDie(tpl_file);
    for (int i = 0; i < tpl_chars.size(); i++){
      char ch = tpl_chars[i];
      ImageRGBA bitmap =
        tpl.Crop32(cfg.BSIZE * i, 0, cfg.BSIZE, cfg.BSIZE, 0x00000000);
      bitmaps.emplace_back(ch, std::move(bitmap));
    }
  }
  
  vector<pair<char, TTF::Char>> chars =
    ParallelMap(bitmaps,
                [&](pair<char, ImageRGBA> letter) {
                  const int codepoint = letter.first;
                  const ImageRGBA &tpl_bitmap = letter.second;

                  ImageA bitmap(cfg.BSIZE, cfg.BSIZE);
                  bitmap.Clear(0);
                  for (int y = 0; y < cfg.BSIZE; y++) {
                    for (int x = 0; x < cfg.BSIZE; x++) {
                      const auto [r, g, b, a] =
                        tpl_bitmap.GetPixel(x, y);
                      bitmap.SetPixel(x, y, r > 0xA0 ? 0xFF : 0x00);
                    }
                  }
                  
                  ImageF sdf(
                      FontProblem::SDFFromBitmap(SDF_CONFIG, bitmap));

                  const auto [unopt_contours, contours] =
                    FontProblem::VectorizeSDF(SDF_CONFIG, sdf);

                  const float right_edge =
                    FontProblem::GuessRightEdge(SDF_CONFIG, sdf);
                  TTF::Char ttf_char =
                    FontProblem::ToChar(SDF_CONFIG, contours, right_edge);

                  /*
                  TTF::MapCoords(
                      [&cfg](float x, float y) {
                        return make_pair(x + 4.0f/36.0f, y);
                      }, &ttf_char);
                  */
                  {
                    MutexLock ml(&out_m);
                    printf("[%c] \n", codepoint);
                  }
                  
                  return make_pair((char)codepoint, ttf_char);
                },
                16);

  TTF::Font font;
  font.baseline = cfg.baseline;
  font.linegap = cfg.linegap;
  font.extra_scale = cfg.extra_scale;
  for (const auto &[c, ch] : chars) font.chars[c] = ch;

  if (font.chars.find(' ') == font.chars.end()) {
    TTF::Char space;
    space.width = cfg.blank_width;
    font.chars[' '] = space;
  }
  
  Util::WriteFile(cfg.filename, font.ToSFD(cfg.font_name, cfg.copyright));
  printf("Wrote %s\n", cfg.filename.c_str());
}

int main(int argc, char **argv) {

  GenerateOne(Config());

  return 0;
}
