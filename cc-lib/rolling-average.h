
// Keeps track of a rolling average of floating point samples.

#ifndef _CC_LIB_ROLLING_AVERAGE_H
#define _CC_LIB_ROLLING_AVERAGE_H

#include <vector>

struct RollingAverage {
  explicit RollingAverage(int max_samples);

  // Add a sample into the pool, and remove the oldest one if we had
  // max_samples samples already.
  void AddSample(double s);

  // Returns the average of the pool of samples. If recompute is true,
  // then this gets the most accurate result, but takes O(max_samples)
  // time. Otherwise, takes O(1) time, but can lose precision if there
  // were ever very large samples in the array (in extreme cases of
  // NaN or infs, it will never recover, in fact).
  //
  // Handles the case that we haven't yet gotten max_samples.
  // Returns NaN when there are no samples yet.
  double Average(bool recompute = false);

  int NumSamples() const;
  
private:
  int max_samples = 0;
  std::vector<double> samples;
  // When samples is of size max_samples, the next index to replace
  // (which represents the oldest sample).
  int next_idx = 0;
  // This is the sum of the samples in the samples array.
  double current_total = 0.0;
};


// Implementations follow.

inline RollingAverage::RollingAverage(int max_samples) :
  max_samples(max_samples) {}

inline void RollingAverage::AddSample(double d) {
  if (samples.size() == max_samples) [[likely]] {
    current_total -= samples[next_idx];
    samples[next_idx] = d;
    current_total += d;
    next_idx++;
    if (next_idx == max_samples) next_idx = 0;
  } else {
    samples.push_back(d);
    current_total += d;
  }
}

inline double RollingAverage::Average(bool recompute) {
  if (samples.empty()) [[unlikely]] {
    return std::numeric_limits<double>::quiet_NaN();
  }
  if (recompute) {
    current_total = 0.0;
    for (double d : samples) current_total += d;
  }
  return current_total / samples.size();
}

inline int RollingAverage::NumSamples() const {
  return samples.size();
}

#endif
