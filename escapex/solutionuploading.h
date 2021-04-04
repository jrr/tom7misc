#ifndef _ESCAPE_SOLUTIONUPLOADING_H
#define _ESCAPE_SOLUTIONUPLOADING_H

#include "escapex.h"
#include "drawable.h"
#include "selector.h"
#include "solution.h"
#include "player.h"

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
