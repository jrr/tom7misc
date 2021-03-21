
#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>
#include <set>

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
  string ops;
  Op(char i, char o, string ops) : input_char(i), output_char(o), ops(ops) {}
};

struct Config {
  string input_font = "helvetica.ttf";

  string font_name = "Steganosaurus";
  string filename = "steganosaurus.sfd";
  string copyright = "Probably useless";

  float extra_scale = 1.0f;
  float linegap = 0.0f;
  float max_gamma = 1.0f;
  float target_frac = 0.0f;
  float blank_width = 1.0f;

  vector<Op> letters;
  Config() {

    int idx = 0;
    std::set<string> done;
    std::function<void(string, int)> Rec = [&](string s, int depth) {
        if (depth == 0) return;
        if (idx == 52) return;

        // Don't end with uppercasing operation.
        if (s.empty() || s.back() != 'u') {
          if (done.find(s) == done.end()) {
            char c = (idx < 26) ? 'a' + idx : 'A' + (idx - 26);
            letters.emplace_back('l', c, s);
            done.insert(s);
            printf("%c: %s\n", c, s.c_str());
            idx++;
          }
        }
        Rec(s + 'l', depth - 1);
        Rec(s + 'u', depth - 1);
      };

    int max_depth = 0;
    while (idx < 52) {
      max_depth++;
      Rec("", max_depth);
    }
    printf("Made config.\n");
  }
};

static void GenerateOne(const Network &make_lowercase,
                        const Network &make_uppercase,
                        Config cfg) {
  TTF ttf(cfg.input_font);

  std::mutex out_m;

  vector<pair<char, TTF::Char>> chars =
    ParallelMap(
        cfg.letters,
        [&](Op op) {
          std::optional<ImageA> sdfo =
            ttf.GetSDF(op.input_char, SDF_CONFIG.sdf_size,
                       SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot,
                       SDF_CONFIG.pad_left,
                       SDF_CONFIG.onedge_value, SDF_CONFIG.falloff_per_pixel);
          CHECK(sdfo.has_value());
          ImageF sdf(sdfo.value());

          for (char c : op.ops) {
            switch (c) {
            case 'u':
              sdf = FontProblem::RunSDFModelF(make_uppercase,
                                              SDF_CONFIG, sdf).first;
              break;
            case 'l':
              sdf = FontProblem::RunSDFModelF(make_lowercase,
                                              SDF_CONFIG, sdf).first;
              break;
            default:
              printf("Bad command in config: %c\n", c);
              CHECK(false);
            }
          }

          constexpr float onedge = SDF_CONFIG.onedge_value / 255.0f;
          
          // Reduce gamma until at least TARGET_FRAC of pixels are
          // above threshold.
          const int target_count =
            cfg.target_frac * (SDF_CONFIG.sdf_size * SDF_CONFIG.sdf_size);
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

            if (count >= target_count) {
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
        }, 16);

  TTF::Font font;
  font.baseline = FontProblem::TTFBaseline(SDF_CONFIG);
  font.linegap = cfg.linegap;
  font.extra_scale = cfg.extra_scale;
  for (const auto &[c, ch] : chars) font.chars[c] = ch;

  Util::WriteFile(cfg.filename, font.ToSFD(cfg.font_name, cfg.copyright));
  printf("Wrote %s\n", cfg.filename.c_str());
}

int main(int argc, char **argv) {

  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));

  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);

  // Generate config.
  GenerateOne(*make_lowercase, *make_uppercase, Config());

  return 0;
}
