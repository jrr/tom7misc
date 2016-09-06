#ifndef __SOLUTIONUPLOADING_H
#define __SOLUTIONUPLOADING_H

#include "escapex.h"
#include "drawable.h"
#include "selector.h"
#include "solution.h"
#include "player.h"
#include "util.h"

#define ALLSOLS_URL "/f/a/escape/allsols/"

struct SolutionUploading {
  static void PromptUpload(Drawable *below,
                           Player *, const string &lmd5,
                           const Solution &s, const string &msg,
                           const string &name,
                           bool speedrec = false);
 private:
  SolutionUploading() = delete;
};

#endif
