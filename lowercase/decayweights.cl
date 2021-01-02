// Defines:
// DECAY_FACTOR, a float like 0.999f, the multiplicative factor
// by which to scale every weight. Biases are not decayed.

// Same kernel used for sparse and dense layers.
__kernel void DecayWeights(// num_nodes * INDICES_PER_NODE,
			   __global float *restrict layer_weights) {
  const int weight_idx = get_global_id(0);
  layer_weights[weight_idx] *= DECAY_FACTOR;
}
