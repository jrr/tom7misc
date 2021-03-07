
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

#include "network.h"
#include "threadutil.h"

using namespace std;

static constexpr FontProblem::SDFConfig SDF_CONFIG = {};

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

using Gen5Result = FontProblem::Gen5Result;
// Gen5Field is one of the 5 fields in Gen5Result with an SDF.
typedef ImageF Gen5Result::*Gen5Field;

int main(int argc, char **argv) {
  TTF ttf("helvetica.ttf");
  // TTF ttf("comic.ttf");

  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));

  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);

  struct Op {
    char input_char;
    char output_char;
    // Field to read for the output SDF.
    // Gen5Result::*field;
    Gen5Field field;
    Op(char i, char o, Gen5Field f) : input_char(i), output_char(o), field(f) {}
  };

  auto IsLetter = [](char c) {
      return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    };

  // Generate config.
  const string FONT_NAME = "Heavenica";
  const string FILENAME = "heavenica.sfd";
  vector<Op> letters;
  for (int i = 0; i < 26; i++) {
    letters.emplace_back('A' + i, 'A' + i, &Gen5Result::up);
    letters.emplace_back('A' + i, 'a' + i, &Gen5Result::up_low);
  }

  // Uppercase punctuation
  for (int i = 32; i < 127; i++) {
    if (!IsLetter(i)) {
      letters.emplace_back(i, i, &Gen5Result::up);
    }
  }

  std::mutex out_m;

  vector<pair<char, TTF::Char>> chars =
    ParallelMap(letters,
                [&](Op op) {
                  std::optional<ImageA> sdfo =
                    ttf.GetSDF(op.input_char, SDF_CONFIG.sdf_size,
                               SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot, SDF_CONFIG.pad_left,
                               SDF_CONFIG.onedge_value, SDF_CONFIG.falloff_per_pixel);

                  if (sdfo.has_value()) {
                    ImageA input_sdf = sdfo.value();

                    FontProblem::Gen5Result gen5result =
                      FontProblem::Gen5(SDF_CONFIG, *make_lowercase, *make_uppercase, input_sdf);

                    ImageF sdf = gen5result.*(op.field);
                    constexpr float onedge = SDF_CONFIG.onedge_value / 255.0f;

                    // Reduce gamma until at least TARGET_FRAC of pixels are above threshold.
                    constexpr float TARGET_FRAC = 0.05;
                    float gamma = 0.95f;
                    bool adjusted = false;
                    while (gamma > 0.01) {
                      int count = 0;
                      for (int y = 0; y < sdf.Height(); y++) {
                        for (int x = 0; x < sdf.Width(); x++) {
                          float f = sdf.GetPixel(x, y);
                          if (powf(f, gamma) >= onedge) count++;
                        }
                      }

                      if (count >= TARGET_FRAC * (SDF_CONFIG.sdf_size * SDF_CONFIG.sdf_size)) {
                        break;
                      }
                      gamma -= 0.01f;
                      adjusted = true;
                    }

                    if (gamma != 1.0f) {
                      if (adjusted) {
                        MutexLock ml(&out_m);
                        printf("[%c] Auto reduced gamma to %.3f\n", op.output_char, gamma);
                      }
                      for (int y = 0; y < sdf.Height(); y++) {
                        for (int x = 0; x < sdf.Width(); x++) {
                          const float f = sdf.GetPixel(x, y);
                          sdf.SetPixel(x, y, powf(f, gamma));
                        }
                      }
                    }

                    const auto [unopt_contours_, contours] =
                      FontProblem::VectorizeSDF(SDF_CONFIG, sdf);
                    const float right_edge =
                      FontProblem::GuessRightEdge(SDF_CONFIG, sdf);
                    TTF::Char ttf_char =
                      FontProblem::ToChar(SDF_CONFIG, contours, right_edge);
                    return make_pair(op.output_char, std::move(ttf_char));
                  } else {
                    {
                      MutexLock ml(&out_m);
                      printf("[%c] Blank input character '%c'\n", op.output_char, op.input_char);
                    }

                    // Blank char, e.g. space.
                    TTF::Char blank_char;
                    blank_char.contours.clear();
                    // maybe more principled to derive this from some char like m?
                    // average?
                    blank_char.width = 0.75f;
                    return make_pair(op.output_char, blank_char);
                  }
               }, 13);

  TTF::Font font;
  font.baseline = FontProblem::TTFBaseline(SDF_CONFIG);
  font.linegap = 0.0f;
  for (const auto [c, ch] : chars)
    font.chars[c] = ch;

  Util::WriteFile(FILENAME, font.ToSFD(FONT_NAME));
  printf("Wrote %s\n", FILENAME.c_str());

  return 0;
}
