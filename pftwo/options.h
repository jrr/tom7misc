
#ifndef __OPTIONS_H
#define __OPTIONS_H

// Tunable parameters with their default values. This should
// grow to include booleans for any doubtful features so that
// we can experiment with removing them.
struct Options {
  // When expanding a node, try this many sequences and
  // choose the best one. "2.25" means 2 (with probability
  // 0.75) or 3 (with probability 0.25).

  // Tuned.
  double num_nexts = 6.7;
  // Tuned.
  double frames_stddev = 75;
  // Tuned.
  double frames_mean = 290.0;

  // Tuned.
  double p_stay_on_node = 0.325;
  // Tuned for headless benchmark, but some evidence that
  // batching is hurting performance overall?
  int node_batch_size = 10;

  int UpdateFrequency() const {
    return 1000 / (node_batch_size * (1.0 / (1.0 - p_stay_on_node)));
  }

  // If true, markov model will be symmetric along the vertical axis
  // P(left) = P(right).
  bool symmetric_markov = true;

  bool use_marathon = false;
  
  // Tune me!
  // Maximum chance of expanding the marathon node when it's eligible.
  double p_expand_marathon = 0.10;

  // Due to threading, the process is inherently random.
  // But this explicitly seeds it to get better randomness.
  int random_seed = 0;
};

#endif
