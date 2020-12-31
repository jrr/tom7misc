
#include "loadfonts.h"

#include <functional>
#include <string>
#include <vector>
#include <shared_mutex>
#include <thread>

#include "base/logging.h"
#include "base/stringprintf.h"

#include "threadutil.h"
#include "arcfour.h"
#include "randutil.h"

#include "fontdb.h"
#include "font-problem.h"
#include "timer.h"

using namespace std;

VectorLoadFonts::VectorLoadFonts(
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

void VectorLoadFonts::Init() {
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
		{
		  ReadMutexLock ml(&fonts_m);
		  if (fonts.size() >= max_fonts)
		    return;
		}
		
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

void VectorLoadFonts::Sync() {
  CHECK(init_thread != nullptr) << "Sync called twice?";
  init_thread->join();
  init_thread.reset();
}

VectorLoadFonts::~VectorLoadFonts() {
  if (init_thread != nullptr) {
    init_thread->join();
    init_thread.reset();
  }
  for (TTF *ttf : fonts) {
    delete ttf;
  }
  fonts.clear();
}



SDFLoadFonts::SDFLoadFonts(
    std::function<bool()> ExitEarly,
    SDFConfig config,
    int max_parallelism,
    int64 max_fonts) : max_parallelism(max_parallelism),
		       max_fonts(max_fonts),
		       ExitEarly(ExitEarly),
		       config(config) {
  fonts.reserve(max_fonts);

  init_thread.reset(new std::thread([this]() {
      this->Init();
    }));
}

void SDFLoadFonts::Init() {
  font_db = std::make_unique<FontDB>();

  vector<string> filenames_todo;
  for (const auto &[filename, info] : font_db->Files()) {
    // Since we render to bitmaps, we allow fonts with more
    // going on (adding messy, decorative).
    if (info.type == FontDB::Type::SERIF ||
	info.type == FontDB::Type::SANS ||
	info.type == FontDB::Type::MESSY ||
	info.type == FontDB::Type::DECORATIVE) {
      if (CaseOK(info)) {
	filenames_todo.push_back(filename);
      }
    }
  }

  // Since this takes a long time (~25 minutes with 30 threads!) to
  // load, do it in a random order so that training isn't biased
  // towards early items in the list.
  ArcFour rc(StringPrintf("%lld", time(nullptr)));
  Shuffle(&rc, &filenames_todo);
  
  printf("%lld eligible fonts\n", (int64)filenames_todo.size());
  Timer timer;
  ParallelApp(filenames_todo,
	      [this, &timer, &filenames_todo](const string &filename) {
		if (ExitEarly()) return;
		{
		  ReadMutexLock ml(&fonts_m);
		  // Note that this strategy can load up to one additional
		  // font per thread...
		  if (fonts.size() >= max_fonts)
		    return;
		}
		
		std::unique_ptr<TTF> ttf{new TTF{filename}};

		Font font;
		
		// Create SDFs up front, since they are expensive.
		// We also require that the SDF is computable for every
		// letter.

		// Make sure ALL letters will fit in training data.
		for (const char c :
		       (string)
		       "abcdefghijklmnopqrstuvwxyz"
		       "ABCDEFGHIJKLMNOPQRSTUVWXYZ") {
		  CHECK(c != 0);
		  std::optional<ImageA> sdf =
		    ttf->GetSDF(c, config.sdf_size,
				config.pad_top, config.pad_bot, config.pad_left,
				config.onedge_value, config.falloff_per_pixel);

		  if (!sdf.has_value()) {
		    {
		      WriteMutexLock ml(&fonts_m);
		      num_failed++;
		    }
			
		    return;
		  }

		  font.sdfs.emplace_back(std::move(sdf.value()));
		}
		
		CHECK(font.sdfs.size() == 26 * 2);
		font.ttf = ttf.release();
		
		{
		  WriteMutexLock ml(&fonts_m);
		  fonts.push_back(std::move(font));
		  if (fonts.size() % 100 == 0) {
		    double ms = timer.MS();
		    double s_per_item = (ms / 1000.0) / fonts.size();
		    printf("Loaded %d, %lld failed in %.2fs (%.2fs/sec; %.2fs remain)..\n",
			   (int)fonts.size(),
			   num_failed,
			   ms / 1000.0,
			   s_per_item,
			   s_per_item * (filenames_todo.size() - fonts.size()));
		  }
		}
	      }, max_parallelism);

  {
    ReadMutexLock ml(&fonts_m);
    printf("Done loading %lld fonts (%lld failed)\n",
	   (int64)fonts.size(), num_failed);
  }
}

void SDFLoadFonts::Sync() {
  CHECK(init_thread != nullptr) << "Sync called twice?";
  init_thread->join();
  init_thread.reset();
}

SDFLoadFonts::~SDFLoadFonts() {
  if (init_thread != nullptr) {
    init_thread->join();
    init_thread.reset();
  }
  for (Font &font : fonts) {
    delete font.ttf;
    font.ttf = nullptr;
  }
  fonts.clear();
}
