
#include "error-history.h"

#include <cstdio>

#include "base/logging.h"
#include "util.h"

using namespace std;

ErrorHistory::ErrorHistory(const std::string &filename,
			   int num_models) : filename(filename),
					     num_models(num_models) {
  Load();
}

void ErrorHistory::Add(int64_t round_number,
		       double error_per_example,
		       bool is_eval,
		       int model_idx) {
  CHECK(round_number >= 0);
  CHECK(model_idx >= 0 && model_idx < num_models) << model_idx << " " << num_models;

  records.push_back(Record{.round_number = round_number,
			   .error_per_example = error_per_example,
			   .model_index = model_idx,
			   .is_eval = is_eval});
}

void ErrorHistory::Save() {
  // XXX sort
  FILE *f = fopen(filename.c_str(), "wb");
  CHECK(f) << filename;

  for (const Record &r : records) {
    fprintf(f, "%lld\t%d\t%c\t%.12g\n",
	    r.round_number, r.model_index,
	    r.is_eval ? 't' : 'f',
	    r.error_per_example);
  }
  
  fclose(f);
  printf("Wrote %lld error records to %s\n", (int64_t)records.size(),
	 filename.c_str());
}

void ErrorHistory::Load() {
  vector<string> lines = Util::ReadFileToLines(filename);
  records.clear();
  records.reserve(lines.size());
  for (string &line : lines) {
    Record r;
    r.round_number = std::stoll(Util::chopto('\t', line));
    if (r.round_number == 0) continue;
    r.model_index = std::stoi(Util::chopto('\t', line));
    if (r.model_index < 0 || r.model_index >= num_models) continue;
    r.is_eval = Util::chopto('\t', line) == "t";
    r.error_per_example = std::stod(Util::chopto('\t', line));
    records.push_back(r);
  }

  printf("Read %lld error records from %s\n",
	 (int64_t)records.size(),
	 filename.c_str());
}
