
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


int main(int argc, char **argv) {
  //  TTF ttf("helvetica.ttf");
  TTF ttf("comic.ttf");

  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));

  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);

  vector<char> letters;
  for (int i = 0; i < 26; i++) letters.push_back('A' + i);

  vector<pair<char, TTF::Char>> chars =
    ParallelMap(letters,
                [&](char c) {
                  std::optional<ImageA> sdfo =
                    ttf.GetSDF(c, SDF_CONFIG.sdf_size,
                               SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot, SDF_CONFIG.pad_left,
                               SDF_CONFIG.onedge_value, SDF_CONFIG.falloff_per_pixel);
                  CHECK(sdfo.has_value());
                  ImageA input_sdf = sdfo.value();

                  FontProblem::Gen5Result gen5result =
                    FontProblem::Gen5(SDF_CONFIG, *make_lowercase, *make_uppercase, input_sdf);

                  const ImageA sdf = gen5result.up;

                  // XXX run network or whatever
                  const auto [unopt_contours_, contours] =
                    FontProblem::VectorizeSDF(SDF_CONFIG, sdf);
                  const float right_edge =
                    FontProblem::GuessRightEdge(SDF_CONFIG, ImageF(sdf));
                  TTF::Char ttf_char =
                    FontProblem::ToChar(SDF_CONFIG, contours, right_edge);
                  return make_pair(c, std::move(ttf_char));
               }, 13);

  TTF::Font font;
  font.baseline = FontProblem::TTFBaseline(SDF_CONFIG);
  for (const auto [c, ch] : chars)
    font.chars[c] = ch;

  Util::WriteFile("font.sfd", font.ToSFD("Tracevetica"));

#if 0
  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));
  FontProblem::Gen5Result gen5result =
    FontProblem::Gen5(SDF_CONFIG, *make_lowercase, *make_uppercase, vector_sdf);
#endif

  return 0;
}
