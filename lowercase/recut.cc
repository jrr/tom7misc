
// Converts the eval output for lower/uppercase sdf to frames, a la
// resequence, but also processing them into the movie I want.

#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"

#include "threadutil.h"
#include "base/stringprintf.h"
#include "util.h"
#include "base/logging.h"
#include "image.h"

using namespace std;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

static bool LOWERCASE = true;

static string Frontslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '\\' ? '/' : c);

  if (ret.find("d:/") == 0) {
    ret[0] = '/';
    ret[1] = 'd';
  } else if (ret.find("c:/") == 0) {
    ret[0] = '/';
    ret[1] = 'c';
  }

  return ret;
}

static string Backslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '/' ? '\\' : c);
  return ret;
}

static void AddAllFiles(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (!Util::isdir(filename)) {
      if (!filename.empty() &&
          // Should probably delete emacs backups..?
          filename[filename.size() - 1] != '#' &&
          filename[filename.size() - 1] != '~') {

        if (LOWERCASE) {
          if (filename.find(".lower.") != string::npos) {
            all_files->push_back(Backslash(filename));
          }
        } else {
          if (filename.find(".upper.") != string::npos) {
            all_files->push_back(Backslash(filename));
          }
        }
      }
    }
  }
}

static ImageRGBA Process(const ImageRGBA &input) {
  static constexpr int LETTER_WIDTH = 64;
  static constexpr int LETTER_X_MARGIN = 2;
  static constexpr int LETTER_HEIGHT = 64;
  static constexpr int LETTER_Y_MARGIN = 2;
  static constexpr int LEFT_MARGIN = 12;
  static constexpr int TOP_MARGIN = 28;

  [[maybe_unused]]
    static constexpr int NUM_ITERS = 12;

  static constexpr int MAX_ITERS = 3;
  
  // letters of iters of 36x36threshold image. 0th is input
  vector<vector<ImageRGBA>> letters;
  for (int letter = 0; letter < 26; letter++) {
    vector<ImageRGBA> col;
    col.reserve(MAX_ITERS);
    for (int iter = 0; iter < MAX_ITERS; iter++) {
      const int startx =
        LEFT_MARGIN + (LETTER_WIDTH + LETTER_X_MARGIN) * letter;
  
      const int starty =
        TOP_MARGIN + iter * (LETTER_HEIGHT + LETTER_Y_MARGIN);
      ImageRGBA letter = input.Crop32(startx, starty, 36, 36);
      col.emplace_back(std::move(letter));
    }
    letters.emplace_back(std::move(col));
  }

  ImageRGBA out(1920, 1080);
  out.Clear32(0x000000FF);
  // Title text
  out.BlendImage(0, 0, input.Crop32(0, 0, 1920, 26));

  constexpr int OUT_TOP_MARGIN = TOP_MARGIN;
  constexpr int OUT_LEFT_MARGIN = -30;
  constexpr int SCALE = 5;
  constexpr int OLAP = 32;
  constexpr int OUT_W = (36 * SCALE - OLAP);
  constexpr int OUT_ROW_H = (36 * SCALE - OLAP) + 4;
  for (int letter = 0; letter < 26; letter++) {
    int base_y = 0;

    if (letter >= 13) base_y = 1080 / 2;
    int base_x = (letter % 13) * OUT_W;

    for (int iter = 0; iter < MAX_ITERS; iter++) { 
      int starty = base_y + OUT_TOP_MARGIN + OUT_ROW_H * iter;
      int startx = base_x + OUT_LEFT_MARGIN;

      CHECK(letter < letters.size());
      CHECK(iter < letters[letter].size());
      out.BlendImage(startx, starty,
                     letters[letter][iter].ScaleBy(SCALE));
    }
  }
  return out;
}

static void Recut(const string &in_dir, const string &out_dir) {
  CHECK(out_dir.find("\\") == string::npos);
  CHECK(out_dir.find("/") == string::npos);  
  vector<string> files;
  AddAllFiles(in_dir, &files);
  std::sort(files.begin(), files.end(),
            [](const string &a, const string &b) {
              return Util::natural_compare(a, b) < 0;
            });
  printf("%lld files\n", (int64)files.size());
  vector<pair<string, string>> remapped;
  remapped.reserve(files.size());
  for (int i = 0; i < files.size(); i++) {
    remapped.emplace_back(files[i],
                          StringPrintf("%s\\frame%d.png", out_dir.c_str(), i));
  }
  printf("Remapped.\n");
  
  // XXX
#if 0
  static constexpr int TRUNCATE = 20;
  if (remapped.size() > TRUNCATE) {
    remapped.resize(TRUNCATE);
  }
#endif
  
  printf("Processing...\n");
  std::mutex out_m;
  int done = 0;
  ParallelApp(remapped,
              [&out_m, &done](const std::pair<string, string> &fs) {
                const auto &[infile, outfile] = fs;
                std::unique_ptr<ImageRGBA> input(ImageRGBA::Load(infile));
                CHECK(input.get() != nullptr) << infile;
                ImageRGBA output = Process(*input);
                CHECK(output.Save(outfile));
                {
                  MutexLock ml(&out_m);
                  done++;
                  if (done % 100 == 0) {
                    printf("%d done\n", done);
                  }
                }
              }, 16);
  printf("All done.\n");
}

int main(int argc, char **argv) {
  CHECK(argc == 3) << "./resequence.exe in_dir out_dir";
  Recut(argv[1], argv[2]);
  
  return 0;
}
