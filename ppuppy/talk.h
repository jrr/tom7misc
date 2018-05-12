
// Data for a "talk." Series of slides, each of which can
// have animations or other special behavior. Talk can
// be "precompiled," since the best quality conversion is
// pretty slow on the pi and we want startup times to be
// as fast as possible.

#ifndef __TALK_H
#define __TALK_H

#include <string>
#include <vector>
#include <utility>

#include "screen.h"
#include "ppuppy.h"

using namespace std;

struct Talk {

  struct Frame {
    // If multi, then compile as two frames with different
    // scroll offsets.
    bool multi = false;
    // Source image.
    string filename;
    int duration = 1;
  };

  struct Slide {
    vector<Frame> anim;
  };

  static Talk Load(const string &filename);
  
  void Save(const string &meta_file,
	    const string &slide_data_file);
  
  vector<Slide> slides;
};

struct CompiledTalk {
  // There is a flat array of screens.
  int NumScreens() {
    return (int)screen_data.size();
  }
  Screen *GetScreen(int n) {
    return &screen_data[n];
  }

  struct Slide {
    // screen idx and duration (> 0)
    vector<std::pair<int, int>> screens;
  };
  
  int NumSlides() {
    return (int)slides.size();
  }

  Slide *GetSlide(int n) {
    return &slides[n];
  }
  
  CompiledTalk(const string &meta_file,
	       const string &slide_data_file);
  
private:
  vector<Screen> screen_data;
  vector<Slide> slides;
};

#endif
