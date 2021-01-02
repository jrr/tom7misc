// Update the weights and biases for each node.

// If noclip is true, then allow updates of arbitrary size.
// This is formally "correct" but sometimes results in unusable
// networks (with infinite weights, etc.). Recommend NOCLIP=false.
#define NOCLIP false

// Alternative (with NOCLIP=true) you can require the weights
// and biases to stay within -MAX to MAX.
#define CONSTRAIN true
#define CONSTRAIN_WEIGHT_MAX 16.0f
#define CONSTRAIN_BIAS_MAX 16384.0f

// Defines:
// INDICES_PER_NODE, an int, the number of input indices
// per node on this layer.
//
// Note this one does not depend on the transfer function.

__kernel void UpdateWeightsSparse(
                 float learning_rate,
                 // int indices_per_node,
                 __global const float *restrict layer_error,
                 // num_nodes * INDICES_PER_NODE
                 __global const int *restrict layer_indices,
                 __global const float *restrict layer_values,
                 // num_nodes * INDICES_PER_NODE,
                 __global float *restrict layer_weights,
                 // num_nodes
                 __global float *restrict layer_biases) {
  const int node_idx = get_global_id(0);
  const float delta_j = layer_error[node_idx];
  const float learning_rate_times_delta_j = learning_rate * delta_j;

  for (int input_idx = 0; input_idx < INDICES_PER_NODE; input_idx++) {
    const int gidx = INDICES_PER_NODE * node_idx + input_idx;
    // Offset of the node, which we use to get its output.
    const int src_idx = layer_indices[gidx];
    const float x_ji = layer_values[src_idx];

    #if NOCLIP
    // PERF: fma()?
    layer_weights[gidx] += learning_rate_times_delta_j * x_ji;

    #elif CONSTRAIN
    // PERF fma()
    const float new_value = layer_weights[gidx] + learning_rate_times_delta_j * x_ji;
    // PERF fmin/fmax
    // if (new_value <= CONSTRAIN_MAX && new_value >= -CONSTRAIN_MAX)
    //      layer_weights[gidx] = new_value;

    const float constrained_value =
      fmax(-CONSTRAIN_WEIGHT_MAX, fmin(CONSTRAIN_WEIGHT_MAX, new_value));
    layer_weights[gidx] = constrained_value;

    #else

    // Clipping
    const float update =
      fmax(-1.0f,
           fmin(1.0f, learning_rate_times_delta_j * x_ji));
    if (!isnan(update))
      layer_weights[gidx] += update;
    #endif
  }

  #if NOCLIP
  layer_biases[node_idx] += learning_rate_times_delta_j;
  #elif CONSTRAIN
  const float new_bias = layer_biases[node_idx] + learning_rate_times_delta_j;
  // if (new_bias <= CONSTRAIN_MAX && new_bias >= -CONSTRAIN_MAX)
  // layer_biases[node_idx] = new_bias;

  const float constrained_value =
    fmax(-CONSTRAIN_BIAS_MAX, fmin(CONSTRAIN_BIAS_MAX, new_bias));
  layer_biases[node_idx] = constrained_value;

  #else
  const float bupdate =
    fmax(-1.0f,
         fmin(1.0f, learning_rate_times_delta_j));
  if (!isnan(bupdate))
    layer_biases[node_idx] += bupdate;
  #endif
}

// When the layer is dense.
__kernel void UpdateWeightsDense(
                 float learning_rate,
                 // int indices_per_node,
                 __global const float *restrict layer_error,
                 // source layer's output values
                 __global const float *restrict layer_values,
                 // num_nodes * INDICES_PER_NODE,
                 __global float *restrict layer_weights,
                 // num_nodes
                 __global float *restrict layer_biases) {
  const int node_idx = get_global_id(0);
  const float delta_j = layer_error[node_idx];
  const float learning_rate_times_delta_j = learning_rate * delta_j;

  for (int input_idx = 0; input_idx < INDICES_PER_NODE; input_idx++) {
    const int gidx = INDICES_PER_NODE * node_idx + input_idx;
    // Offset of the node, which we use to get its output.
    // const int src_idx = layer_indices[gidx];
    // For a dense layer, each source node is used in order.
    const int src_idx = input_idx;

    const float x_ji = layer_values[src_idx];

    #if NOCLIP
    // PERF: fma()?
    layer_weights[gidx] += learning_rate_times_delta_j * x_ji;

    #elif CONSTRAIN
    // PERF fma()
    const float new_value = layer_weights[gidx] + learning_rate_times_delta_j * x_ji;
    // PERF fmin/fmax
    const float constrained_value =
      fmax(-CONSTRAIN_WEIGHT_MAX, fmin(CONSTRAIN_WEIGHT_MAX, new_value));
    layer_weights[gidx] = constrained_value;

    #else
    // Clipping
    const float update =
      fmax(-1.0f,
           fmin(1.0f, learning_rate_times_delta_j * x_ji));
    if (!isnan(update))
      layer_weights[gidx] += update;
    #endif
  }

  #if NOCLIP
  layer_biases[node_idx] += learning_rate_times_delta_j;
  #elif CONSTRAIN
  const float new_bias = layer_biases[node_idx] + learning_rate_times_delta_j;
  // if (new_bias <= CONSTRAIN_MAX && new_bias >= -CONSTRAIN_MAX)
  //    layer_biases[node_idx] = new_bias;

  const float constrained_value =
    fmax(-CONSTRAIN_BIAS_MAX, fmin(CONSTRAIN_BIAS_MAX, new_bias));
  layer_biases[node_idx] = constrained_value;



  #else
  const float bupdate =
    fmax(-1.0f,
         fmin(1.0f, learning_rate_times_delta_j));
  if (!isnan(bupdate))
    layer_biases[node_idx] += bupdate;
  #endif
}
