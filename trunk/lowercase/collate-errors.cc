
#include "error-history.h"

// XXX should be configurable on command line
static constexpr int NUM_MODELS = 2;
static constexpr int MAX_POINTS = 10000;

int main(int argc, char **argv) {

  ErrorHistory history("sdf-error.tsv", NUM_MODELS);

  history.WriteMergedTSV("sdf-merged.tsv", {MAX_POINTS});

  return 0;
}
