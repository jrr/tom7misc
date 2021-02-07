
#include "error-history.h"

#include <cstdio>
#include <optional>
#include <map>
#include <vector>
#include <string>

#include "base/logging.h"
#include "util.h"

using namespace std;
using int64 = int64_t;

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

void ErrorHistory::WriteMergedTSV(const string &outfile,
                                  std::optional<int> max_points) const {
  std::map<int64, vector<Record>> collated_records;
  for (const auto &record : records)
    collated_records[record.round_number].push_back(record);
  
  vector<bool> had_error(num_models, false);
  auto AllHadError = [&had_error]() {
      for (bool b : had_error) if (!b) return false;
      return true;
    };
  vector<double> last_error(num_models, 0.0);

  // Collate so that we have one row for each round.
  vector<std::pair<int64, vector<double>>> rows;
  for (const auto &[round, recs] : collated_records) {
    for (const auto &rec : recs) {
      // TODO: Also output eval error?
      if (!rec.is_eval) {
        int idx = rec.model_index;
        had_error[idx] = true;
        last_error[idx] = rec.error_per_example;
      }
    }
    if (AllHadError()) {
      rows.emplace_back(round, last_error);
    }
  }

  // Now thin to the number of rows.
  if (max_points.has_value() && rows.size() > max_points.value()) {
    const int64 out_points = max_points.value();

    CHECK(rows.size() >= 2) << "need at least two rows";
    #if 0
    const int64 round_min = rows[0].first;
    const int64 round_max = rows[rows.size() - 1].first;
    const int64 span = round_max - round_min;
    CHECK(span > 0) << "rounds not sorted!?";
    const double ival = span / (double)out_points;
    #endif
    
    vector<std::pair<int64, vector<double>>> out;
    out.reserve(out_points);

    // First point is always the actual first point.
    int prev_srci = 0;
    out.push_back(rows[0]);
    // Now, for the rest, assume points are evenly distributed in
    // the input (not necessarily true; could improve by finding
    // the target round) to compute the index to sample.
    for (int i = 1; i < out_points; i++) {
      // How far are we through the output?
      double f = i / (double)out_points;
      // And how far is that through the original rows?
      const int srci = f * rows.size();
      if (srci < 0 || srci >= rows.size()) break;

      // This is not really right either, in the case that we
      // have more samples towards the end of the time period.
      vector<double> avgs(num_models, 0.0);
      double denom = 0.0;
      for (int j = prev_srci + 1; j <= srci; j++) {
        const auto &[row_round, row_vals] = rows[j];
        for (int m = 0; m < num_models; m++) {
          avgs[m] += row_vals[m];
        }
        denom += 1.0;
      }
      for (double &v : avgs) v /= denom;

      // Round could be the midpoint?
      out.emplace_back(rows[srci].first, avgs);
      prev_srci = srci;
    }

    rows = std::move(out);
  }
  
  FILE *f = fopen(outfile.c_str(), "wb");
  for (const auto &[round, vals] : rows) {
    fprintf(f, "%lld", round);
    for (double v : vals)
      fprintf(f, "\t%.4f", v);
    fprintf(f, "\n");
  }
  fclose(f);
  printf("Wrote merged to %s\n", outfile.c_str());
}
