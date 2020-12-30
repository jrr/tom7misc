
#include "loadfonts.h"

#include <functional>
#include <string>
#include <vector>
#include <shared_mutex>
#include <thread>

#include "threadutil.h"
#include "fontdb.h"
#include "font-problem.h"

using namespace std;

LoadFonts::LoadFonts(
    std::function<bool()> ExitEarly,
    const vector<int> &row_max_points,
    int max_parallelism,
    int64 max_fonts) : max_parallelism(max_parallelism),
		       max_fonts(max_fonts),
		       ExitEarly(ExitEarly),
		       row_max_points(row_max_points) {
  fonts.reserve(max_fonts);

  init_thread.reset(new std::thread([this]() {
      this->Init();
    }));
}

static bool CaseOK(const FontDB::Info &info) {
  // Might be hand-labeled. SAME_CASE means case is not ok.
  auto it = info.flags.find(FontDB::Flag::SAME_CASE);
  if (it != info.flags.end())
    return !it->second;

  // Otherwise, use classifier.
  // No data? reject.
  if (info.bitmap_diffs < 0) return false;
  
  // Threshold comes from PR curve where precision first
  // dips below 90%. (Recall is about 80%).
  static constexpr float THRESHOLD = 0.06563;
  if (info.bitmap_diffs < THRESHOLD)
    return false;

  return true;
}

void LoadFonts::Init() {
  font_db = std::make_unique<FontDB>();

  vector<string> filenames_todo;
  for (const auto &[filename, info] : font_db->Files()) {
    // XXX make the filtering criteria configurable
    if (info.type == FontDB::Type::SERIF ||
	info.type == FontDB::Type::SANS) {
      if (CaseOK(info)) {
	filenames_todo.push_back(filename);
      }
    }
  }

  printf("%lld eligible fonts\n", (int64)filenames_todo.size());
  ParallelApp(filenames_todo,
	      [this](const string &filename) {
		if (ExitEarly()) return;
		
		TTF *ttf = new TTF{filename};

		vector<float> v;
		v.resize(
		    FontProblem::BufferSizeForPoints(row_max_points));
		
		// Make sure ALL letters will fit in training data.
		for (int c = 0; c < 26; c++) {
		  int upper = 'A' + c;
		  int lower = 'a' + c;
		  if (!FontProblem::FillVector(ttf, upper, row_max_points,
					       v.data()) ||
		      !FontProblem::FillVector(ttf, lower, row_max_points,
					       v.data())) {
		    delete ttf;
		    return;
		  }
		}
		  
		// XXX other filters
		{
		  WriteMutexLock ml(&fonts_m);
		  fonts.push_back(ttf);
		}
	      }, max_parallelism);

  printf("Done loading %lld fonts\n", (int64)fonts.size());
}

void LoadFonts::Sync() {
  CHECK(init_thread != nullptr) << "Sync called twice?";
  init_thread->join();
  init_thread.reset();
}

LoadFonts::~LoadFonts() {
  if (init_thread != nullptr) {
    init_thread->join();
    init_thread.reset();
  }
  for (TTF *ttf : fonts) {
    delete ttf;
  }
  fonts.clear();
}
