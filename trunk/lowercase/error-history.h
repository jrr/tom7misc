#ifndef _LOWERCASE_ERROR_HISTORY_H
#define _LOWERCASE_ERROR_HISTORY_H

#include <string>
#include <vector>
#include <cstdint>
#include <optional>

// Stores error history (train, test) over time. Multiple models are
// supported. Since we typically train a model over O(100k) rounds, we
// just keep all of the error history in memory.
//
// Not thread-safe.
struct ErrorHistory {

  ErrorHistory(const std::string &filename,
               int num_models = 1);

  // OK to add rounds sparsely, or even out-of-order.
  // is_eval false means this is training error, true means test (aka eval)
  // 0 <= model_idx < num_models.
  void Add(int64_t round_number,
           double error_per_example,
           bool is_eval,
           int model_idx = 0);

  void Save();

  void WriteMergedTSV(const std::string &outfile,
                      std::optional<int> max_points = {}) const;
  
private:
  void Load();
  
  const std::string filename;
  const int num_models = 0;
  
  struct Record {
    int64_t round_number = 0;
    double error_per_example = 0.0;
    int model_index = 0;
    bool is_eval = false;
  };

  std::vector<Record> records;
};

#endif
