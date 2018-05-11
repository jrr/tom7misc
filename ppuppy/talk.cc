#include "talk.h"

#include <memory>
#include <utility>
#include <string>
#include <vector>
#include <string.h>

#include "screen.h"
#include "util.h"
#include "base/logging.h"
#include "arcfour.h"
#include "convert.h"
#include "base/stringprintf.h"

Talk Talk::Load(const string &src_filename) {
  vector<string> src = Util::ReadFileToLines(src_filename);
  CHECK(!src.empty()) << src_filename << " empty / not found?";

  Talk talk;
  Slide *slide = nullptr;

  static constexpr int DEFAULT_DURATION = 2;
  static constexpr bool DEFAULT_MULTI = false;
  int duration = DEFAULT_DURATION;
  bool multi = DEFAULT_MULTI;
  for (string line : src) {
    string cmd = Util::chop(line);
    if (cmd == "#" || cmd == "")
      continue;

    auto AddFile = [&slide, &multi, &duration](const string &f) {
      slide->anim.emplace_back();
      Frame *frame = &slide->anim.back();
      frame->multi = multi;
      frame->filename = f;
      frame->duration = duration;
    };
    
    if (cmd == "slide") {
      talk.slides.emplace_back();
      slide = &talk.slides.back();
      duration = DEFAULT_DURATION;
      multi = DEFAULT_MULTI;
    } else if (cmd == "dur") {
      string d = Util::chop(line);
      CHECK(slide != nullptr) << "start slide first";
      duration = atoi(d.c_str());
      CHECK(duration > 0) << "got: " << d;
    } else if (cmd == "multi") {
      CHECK(slide != nullptr) << "start slide first";
      multi = true;
    } else if (cmd == "single") {
      CHECK(slide != nullptr) << "start slide first";
      multi = false;
    } else if (cmd == "array") {
      CHECK(slide != nullptr) << "start slide first";
      string los = Util::chop(line);
      string his = Util::chop(line);
      string f = Util::chop(line);
      CHECK(!f.empty() && f.find("%") != string::npos) << "array: " << f;
      int lo = atoi(los.c_str());
      int hi = atoi(his.c_str());
      for (int x = lo; x <= hi; x++) {
	AddFile(Util::Replace(f, "%", StringPrintf("%d", x)));
      }
    } else {
      // Command must be a file that we can open.
      CHECK(slide != nullptr) << "start slide first (" << cmd << ")";
      AddFile(cmd);
    }
  }

  return talk;
}

void Talk::Save(const string &meta_file,
		const string &slide_data_file) {
  using CT = CompiledTalk;

  vector<CT::Slide> cslides;

  // TODO: Cache of converted slides.
  vector<Screen> screens;
  auto AddScreen = [&screens](Screen screen) -> int {
    int idx = screens.size();
    screens.push_back(screen);
    return idx;
  };
  ArcFour rc("compile-talk");
  
  for (const Slide &slide : slides) {
    // The animation gets flattened.
    CT::Slide cslide;
    CHECK(!slide.anim.empty());
    for (const Frame &frame : slide.anim) {
      CHECK(frame.duration > 0);
      std::unique_ptr<ImageRGB> img(ImageRGB::Load(frame.filename));
      CHECK(img.get()) << frame.filename;
      fprintf(stderr, "%s\n", frame.filename.c_str());
      if (frame.multi) {
	CHECK(false) << "frame.multi unimplemented";
      } else {
	Screen screen;
	MakePalette(PaletteMethod::GREEDY_BIGRAMS,
		    img.get(), &rc, &screen);
	FillScreenSelective(img.get(), &screen);
	int idx = AddScreen(screen);
	cslide.screens.emplace_back(idx, frame.duration);
      }
    }
    cslides.push_back(cslide);
  }

  {
    string outdata;
    int bytes = screens.size() * sizeof (Screen);
    outdata.resize(bytes);
    for (int i = 0; i < screens.size(); i++) {
      memcpy(&outdata[i * sizeof (Screen)], &screens[i], sizeof (Screen));
    }
    Util::WriteFile(slide_data_file, outdata);
    fprintf(stderr, "Wrote %d bytes to %s\n", bytes, slide_data_file.c_str());
    fprintf(stderr, "sizeof (Screen) = %d\n", (int)sizeof (Screen));
  }

  FILE *metaf = fopen(meta_file.c_str(), "wb");
  CHECK(metaf) << meta_file;

  for (const CT::Slide &cslide : cslides) {
    for (const pair<int, int> s : cslide.screens) {
      fprintf(metaf, "%d %d   ", s.first, s.second);
    }
    fprintf(metaf, "\n");
  }
  fclose(metaf);
}


CompiledTalk::CompiledTalk(const string &meta_file,
			   const string &slide_data_file) {
  vector<string> meta = Util::ReadFileToLines(meta_file);
  string data = Util::ReadFile(slide_data_file);

  CHECK((data.size() % sizeof (Screen)) == 0) << slide_data_file <<
    ": Expected multiple of " << (int)sizeof (Screen) << " but got " <<
    (int)data.size();

  int num_screens = data.size() / sizeof (Screen);
  screen_data.resize(num_screens);
  for (int i = 0; i < num_screens; i++) {
    memcpy(&screen_data[i],
	   data.data() + (i * sizeof (Screen)), sizeof (Screen));
  }

  slides.reserve(meta.size());
  for (string line : meta) {
    Slide slide;
    while (!line.empty()) {
      string idxs = Util::chop(line);
      if (idxs.empty()) break;
      string durs = Util::chop(line);
      int idx = atoi(idxs.c_str());
      int dur = atoi(durs.c_str());
      CHECK(dur > 0) << durs << "\nin\n" << line;
      slide.screens.emplace_back(idx, dur);
    }
    printf("Slide with %d frames.\n", (int)slide.screens.size());
    slides.push_back(std::move(slide));
  }
}

