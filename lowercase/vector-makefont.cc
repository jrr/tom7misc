
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
  string input_font;

  string font_name = "Heavenica";
  string filename = "heavenica.sfd";
  float extra_scale = 1.0f;
  float linegap = 0.0f;
  float blank_width = 1.0f;

  vector<Op> letters;
};

static bool IsLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

// Lowerercase Franklin Gothic
Config Franklin() {
  Config cfg;
  cfg.input_font = "franklin.ttf";
  cfg.font_name = "Franklin TODO CLEVER NAME";
  cfg.filename = "franklow.sfd";
  /*
  cfg.extra_scale = 1.2f;
  cfg.linegap = -0.1f;
  */
  cfg.blank_width = 0.75f;

  for (int i = 32; i < 127; i++) {
    cfg.letters.emplace_back(i, i);
  }
  return cfg;
}

// Lowerercase Franklin Gothic
Config Futurda() {
  Config cfg;
  cfg.input_font = "futura.ttf";
  cfg.font_name = "Futurda";
  cfg.filename = "futurda.sfd";
  /*
  cfg.extra_scale = 1.2f;
  cfg.linegap = -0.1f;
  */
  cfg.blank_width = 0.75f;

  for (int i = 32; i < 127; i++) {
    cfg.letters.emplace_back(i, i);
  }
  return cfg;
}


// Vector version just has one network, because I only trained one direction.
static void GenerateOne(const Network &net, Config cfg) {
  TTF ttf(cfg.input_font);

  std::mutex out_m;

  vector<pair<char, TTF::Char>> chars =
    ParallelMap(cfg.letters,
                [&](Op op) {

                  const int codepoint = op.input_char;
                  Stimulation stim{net};
                  // XXX could just skip if it's not a letter?
                  if (FontProblem::FillVector(
                          &ttf, codepoint, vector<int>(row_max_points),
                          stim.values[0].data())) {
                  net.RunForward(&stim);
                  TTF::Char ch;
                  ch.contours = FontProblem::VectorGetContours(row_max_points,
                                                               stim.values.back());
                  FontProblem::GuessWidth(&ch);

                  return make_pair(op.output_char, ch);

                  } else {
                    {
                      MutexLock ml(&out_m);
                      printf("%s [%c] Input font does not fit\n",
                             cfg.font_name.c_str(),
                             op.input_char);
                    }

                    CHECK(!IsLetter(op.input_char)) << "(aborting because we require all "
                      "the letters, at least)";

                    // Generate a blank char, e.g. space.
                    // Could also consider just copying the input char?
                    TTF::Char blank_char;
                    blank_char.contours.clear();
                    // maybe more principled to derive this from some char like m?
                    // average?
                    blank_char.width = cfg.blank_width;
                    return make_pair(op.output_char, blank_char);
                  }
               }, 13);

  TTF::Font font;
  font.baseline = ttf.Baseline();
  font.linegap = cfg.linegap;
  font.extra_scale = cfg.extra_scale;
  for (const auto &[c, ch] : chars) font.chars[c] = ch;

  Util::WriteFile(cfg.filename, font.ToSFD(cfg.font_name));
  printf("Wrote %s\n", cfg.filename.c_str());

}

int main(int argc, char **argv) {

  std::unique_ptr<Network> make_lowercase;
  make_lowercase.reset(Network::ReadNetworkBinary("first-vectornet/net.val"));

  CHECK(make_lowercase.get() != nullptr);

  // GenerateOne(*make_lowercase, Franklin());
  GenerateOne(*make_lowercase, Futurda());

  return 0;
}
