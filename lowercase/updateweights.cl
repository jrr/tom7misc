// Update the weights and biases for each node.

// If noclip is true, then allow updates of arbitrary size.
// This is formally "correct" but sometimes results in unusable
// networks (with infinite weights, etc.). Recommend NOCLIP=false.
#define NOCLIP false

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
    #else
    // Clipping
    const float update =
      fmax(-1.0f,
           fmin(1.0f, learning_rate_times_delta_j * x_ji));
    layer_weights[gidx] += update;
    #endif
  }

  #if NOCLIP
  layer_biases[node_idx] += learning_rate_times_delta_j;
  #else
  const float bupdate =
    fmax(-1.0f,
         fmin(1.0f, learning_rate_times_delta_j));
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
    #else
    // Clipping
    const float update =
      fmax(-1.0f,
           fmin(1.0f, learning_rate_times_delta_j * x_ji));
    layer_weights[gidx] += update;
    #endif
  }

  #if NOCLIP
  layer_biases[node_idx] += learning_rate_times_delta_j;
  #else
  const float bupdate =
    fmax(-1.0f,
         fmin(1.0f, learning_rate_times_delta_j));
  layer_biases[node_idx] += bupdate;
  #endif
}
