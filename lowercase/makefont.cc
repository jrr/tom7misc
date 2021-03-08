
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

struct Op {
  char input_char;
  char output_char;
  // Field to read for the output SDF.
  // Gen5Result::*field;
  Gen5Field field;
  Op(char i, char o, Gen5Field f) : input_char(i), output_char(o), field(f) {}
};

struct Config {
  string input_font;

  string font_name = "Heavenica";
  string filename = "heavenica.sfd";
  float extra_scale = 1.0f;
  float linegap = 0.0f;
  float max_gamma = 1.0f;
  float target_frac = 0.0f;
  float blank_width = 1.0f;

  vector<Op> letters;
};

static bool IsLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

// Upperercase Helvetica
Config Heavenica() {
  Config cfg;
  cfg.input_font = "helvetica.ttf";
  cfg.font_name = "Heavenica";
  cfg.filename = "heavenica.sfd";
  cfg.extra_scale = 1.2f;
  cfg.linegap = -0.1f;
  cfg.max_gamma = 0.95f;
  cfg.target_frac = 0.05f;
  cfg.blank_width = 0.75f;

  for (int i = 0; i < 26; i++) {
    cfg.letters.emplace_back('A' + i, 'A' + i, &Gen5Result::up);
    cfg.letters.emplace_back('A' + i, 'a' + i, &Gen5Result::up_low);
  }

  // Uppercase punctuation
  for (int i = 32; i < 127; i++) {
    if (!IsLetter(i)) {
      cfg.letters.emplace_back(i, i, &Gen5Result::up);
    }
  }
  return cfg;
}

// Lowerercase Helvetica
Config SpezialHellvetica() {
  Config cfg;
  cfg.input_font = "helvetica.ttf";
  cfg.font_name = "Spezial Hellvetica";
  cfg.filename = "spezial-hellvetica.sfd";
  // XXX tweak
  cfg.extra_scale = 1.3f;
  cfg.linegap = -0.05f;
  cfg.max_gamma = 0.975f;
  cfg.target_frac = 0.025f;
  cfg.blank_width = 0.65f;

  for (int i = 0; i < 26; i++) {
    cfg.letters.emplace_back('a' + i, 'a' + i, &Gen5Result::low);
    cfg.letters.emplace_back('a' + i, 'A' + i, &Gen5Result::low_up);
  }

  // Uppercase punctuation
  for (int i = 32; i < 127; i++) {
    if (!IsLetter(i)) {
      cfg.letters.emplace_back(i, i, &Gen5Result::low);
    }
  }
  return cfg;
}

// Upperercase Times
Config ExponentialNewRoman() {
  Config cfg;
  cfg.input_font = "times.ttf";
  cfg.font_name = "Exponential New Roman";
  cfg.filename = "expnewroman.sfd";
  cfg.extra_scale = 1.2f;
  cfg.linegap = -0.1f;
  cfg.max_gamma = 0.95f;
  cfg.target_frac = 0.05f;
  cfg.blank_width = 0.75f;

  for (int i = 0; i < 26; i++) {
    cfg.letters.emplace_back('A' + i, 'A' + i, &Gen5Result::up);
    cfg.letters.emplace_back('A' + i, 'a' + i, &Gen5Result::up_low);
  }

  // Uppercase punctuation
  for (int i = 32; i < 127; i++) {
    if (!IsLetter(i)) {
      cfg.letters.emplace_back(i, i, &Gen5Result::up);
    }
  }
  return cfg;
}

// Lowerercase Times
Config PlusNewRoman() {
  Config cfg;
  cfg.input_font = "times.ttf";
  cfg.font_name = "Plus New Roman";
  cfg.filename = "plusnewroman.sfd";
  // XXX tweak
  cfg.extra_scale = 1.3f;
  cfg.linegap = -0.05f;
  cfg.max_gamma = 0.975f;
  cfg.target_frac = 0.025f;
  cfg.blank_width = 0.65f;

  for (int i = 0; i < 26; i++) {
    cfg.letters.emplace_back('a' + i, 'a' + i, &Gen5Result::low);
    cfg.letters.emplace_back('a' + i, 'A' + i, &Gen5Result::low_up);
  }

  // Uppercase punctuation
  for (int i = 32; i < 127; i++) {
    if (!IsLetter(i)) {
      cfg.letters.emplace_back(i, i, &Gen5Result::low);
    }
  }
  return cfg;
}


static void GenerateOne(const Network &make_lowercase,
                        const Network &make_uppercase,
                        Config cfg) {
  TTF ttf(cfg.input_font);

  std::mutex out_m;

  vector<pair<char, TTF::Char>> chars =
    ParallelMap(cfg.letters,
                [&](Op op) {
                  std::optional<ImageA> sdfo =
                    ttf.GetSDF(op.input_char, SDF_CONFIG.sdf_size,
                               SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot, SDF_CONFIG.pad_left,
                               SDF_CONFIG.onedge_value, SDF_CONFIG.falloff_per_pixel);

                  if (sdfo.has_value()) {
                    ImageA input_sdf = sdfo.value();

                    FontProblem::Gen5Result gen5result =
                      FontProblem::Gen5(SDF_CONFIG, make_lowercase, make_uppercase, input_sdf);

                    ImageF sdf = gen5result.*(op.field);
                    constexpr float onedge = SDF_CONFIG.onedge_value / 255.0f;

                    // Reduce gamma until at least TARGET_FRAC of pixels are above threshold.
                    float gamma = cfg.max_gamma;
                    bool adjusted = false;
                    while (gamma > 0.01) {
                      int count = 0;
                      for (int y = 0; y < sdf.Height(); y++) {
                        for (int x = 0; x < sdf.Width(); x++) {
                          float f = sdf.GetPixel(x, y);
                          if (powf(f, gamma) >= onedge) count++;
                        }
                      }

                      if (count >= cfg.target_frac * (SDF_CONFIG.sdf_size * SDF_CONFIG.sdf_size)) {
                        break;
                      }
                      gamma -= 0.01f;
                      adjusted = true;
                    }

                    if (gamma != 1.0f) {
                      if (adjusted) {
                        MutexLock ml(&out_m);
                        printf("%s [%c] Auto reduced gamma to %.3f\n",
                               cfg.font_name.c_str(),
                               op.output_char, gamma);
                      }
                      for (int y = 0; y < sdf.Height(); y++) {
                        for (int x = 0; x < sdf.Width(); x++) {
                          const float f = sdf.GetPixel(x, y);
                          sdf.SetPixel(x, y, powf(f, gamma));
                        }
                      }
                    }

                    const auto [unopt_contours, contours] =
                      FontProblem::VectorizeSDF(SDF_CONFIG, sdf);
                    const float right_edge =
                      FontProblem::GuessRightEdge(SDF_CONFIG, sdf);
                    TTF::Char ttf_char =
                      FontProblem::ToChar(SDF_CONFIG, contours, right_edge);
                    return make_pair(op.output_char, std::move(ttf_char));
                  } else {
                    {
                      MutexLock ml(&out_m);
                      printf("%s [%c] Blank input character '%c'\n",
                             cfg.font_name.c_str(),
                             op.output_char, op.input_char);
                    }

                    // Blank char, e.g. space.
                    TTF::Char blank_char;
                    blank_char.contours.clear();
                    // maybe more principled to derive this from some char like m?
                    // average?
                    blank_char.width = cfg.blank_width *
                      FontProblem::TTFBaseline(SDF_CONFIG);
                    return make_pair(op.output_char, blank_char);
                  }
               }, 13);

  TTF::Font font;
  font.baseline = FontProblem::TTFBaseline(SDF_CONFIG);
  font.linegap = cfg.linegap;
  font.extra_scale = cfg.extra_scale;
  for (const auto &[c, ch] : chars) font.chars[c] = ch;

  Util::WriteFile(cfg.filename, font.ToSFD(cfg.font_name));
  printf("Wrote %s\n", cfg.filename.c_str());

}

int main(int argc, char **argv) {

  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));

  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);


  // Generate config.
  // GenerateOne(*make_lowercase, *make_uppercase, Heavenica());
  // GenerateOne(*make_lowercase, *make_uppercase, SpezialHellvetica());
  // GenerateOne(*make_lowercase, *make_uppercase, ExponentialNewRoman());
  GenerateOne(*make_lowercase, *make_uppercase, PlusNewRoman());

  return 0;
}
