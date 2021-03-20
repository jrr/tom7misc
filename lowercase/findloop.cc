
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
static constexpr int SDF_SIZE = SDF_CONFIG.sdf_size;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

struct HashImageA {
  std::size_t operator()(const ImageA &img) const {
    return img.Hash();
  }
};

vector<bool> Threshold(const ImageF &a) {
  vector<bool> ret;
  ret.reserve(a.Width() * a.Height());
  for (int y = 0; y < a.Height(); y++) {
    for (int x = 0; x < a.Width(); x++) {
      ret.push_back(a.GetPixel(x, y) > (SDF_CONFIG.onedge_value / 255.0f));
    }
  }
  return ret;
}

static void FindOneLoop(bool lower, char ch) {
  TTF helvetica("helvetica.ttf");

  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));

  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);

  // std::unordered_map<ImageA, int, HashImageA> seen;
  std::unordered_map<vector<bool>, int> seen;  

  const Network *net =
    lower ? make_lowercase.get() : make_uppercase.get();

  std::optional<ImageA> sdfo =
    helvetica.GetSDF(ch, SDF_CONFIG.sdf_size,
                     SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot,
                     SDF_CONFIG.pad_left,
                     SDF_CONFIG.onedge_value,
                     SDF_CONFIG.falloff_per_pixel);
  CHECK(sdfo.has_value());
  ImageF sdf(sdfo.value());
  // seen[sdf] = 0;

  Timer timer;
  int64 iters = 0;
  while (iters < 25000000) {

    sdf = FontProblem::RunSDFModelF(*net, SDF_CONFIG, sdf).first;
    iters++;

    constexpr float SCALE = 2;
    vector<bool> bits =
      SCALE == 1 ? Threshold(sdf) :
      Threshold(sdf.ResizeBilinear(SDF_SIZE * SCALE,
                                   SDF_SIZE * SCALE));
    
    int &prev = seen[bits];
    if (prev == 0) {
      prev = iters;
    } else {
      printf("Got loop at %d -> %d\n", iters, prev);
      sdf.Make8Bit().GreyscaleRGBA().Save("loop.png");
      return;
    }
    if (iters % 10000 == 0) {
      double total_ms = timer.MS();
      double ips = iters / (total_ms / 1000.0);
      printf("%d iters, %.2f iters/sec\n", iters, ips);
    }
  }
  printf("Never found a loop!");
  sdf.Make8Bit().GreyscaleRGBA().Save("deeeep.png");
  return;
}

static void FindAllLoops(bool lower) {
  TTF helvetica("helvetica.ttf");
  constexpr float SCALE = 2.0;
  
  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));

  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);

  // std::unordered_map<ImageA, int, HashImageA> seen;
  std::unordered_map<vector<bool>, std::vector<std::pair<int, char>>> seen;  

  const Network *net =
    lower ? make_lowercase.get() : make_uppercase.get();

  const string src = lower ?
    "abcdefghijklmnopqrstuvwxyz" :
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  Timer timer;
  int64 all_iters = 0;
  int64 max_depth = 0;
  for (char ch : src) {
    std::optional<ImageA> sdfo =
      helvetica.GetSDF(ch, SDF_CONFIG.sdf_size,
                       SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot,
                       SDF_CONFIG.pad_left,
                       SDF_CONFIG.onedge_value,
                       SDF_CONFIG.falloff_per_pixel);
    CHECK(sdfo.has_value());
    ImageF sdf(sdfo.value());

    int64 iters = 0;
    while (iters < 25000000) {
      
      vector<bool> bits =
        SCALE == 1 ? Threshold(sdf) :
        Threshold(sdf.ResizeBilinear(SDF_SIZE * SCALE,
                                     SDF_SIZE * SCALE));
    
      std::vector<std::pair<int, char>> &prev = seen[bits];
      if (prev.empty()) {
        prev.emplace_back(iters, ch);
      } else {
        auto AddSelf = [&prev, iters, ch]() -> bool {
            for (const auto [previ, prevc] : prev) {
              if (prevc == ch) {
                printf("[%c] full loop %d -> %d\n", ch, iters, previ);
                return true;
              }
            }
            prev.emplace_back(iters, ch);
            return false;
          };
        
        if (AddSelf()) {
          max_depth = std::max(iters, max_depth);
          break;
        }
      }
      if (all_iters % 10000 == 0) {
        double total_ms = timer.MS();
        double ips = all_iters / (total_ms / 1000.0);
        printf("[%c] %d iters %d total, %.2f iters/sec\n",
               ch,
               iters, all_iters, ips);
      }

      CHECK(sdf.Width() == SDF_CONFIG.sdf_size) << sdf.Width();
      CHECK(sdf.Height() == SDF_CONFIG.sdf_size);      
      sdf = FontProblem::RunSDFModelF(*net, SDF_CONFIG, sdf).first;
      iters++;
      all_iters++;
    }

    if (iters >= 25000000) {
      printf("Never found a loop for %c!", ch);
      sdf.Make8Bit().GreyscaleRGBA().Save("deeeep.png");
      return;
    }
  }

  printf("Found loops for all chars. max_depth: %lld\n",
         max_depth);
}


int main(int argc, char **argv) {
  FindOneLoop(true, 'e');
  // FindAllLoops(false);
  return 0;
}

/*
  for helvetica 'q' , make_uppercase,
25000000 iters
Never found a loop!
real    1342m17.718s
user    0m0.015s
sys     0m0.000s

bitmasks:
SCALE=1, cycle of length 2
SCALE=2, cycle of 377->245
SCALE=3, cycle of 2957->2291
SCALE=4, 266.70 iters/sec Got loop at 10899 -> 6472
SCALE=5, 478.44 iters/sec Got loop at 14894 -> 2974
SCALE=6, 440.93 iters/sec Got loop at 26873 -> 16879
SCALE=7, 188.95 iters/sec Got loop at 41187 -> 28329

*/
