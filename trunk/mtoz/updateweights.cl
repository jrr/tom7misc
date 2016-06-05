__kernel void UpdateWeights(float learning_rate,
                            int indices_per_node,
                            __global const float *layer_error,
                            // num_nodes * indices_per_node
                            __global const int *layer_indices,
                            __global const float *layer_values,
                            // num_nodes * indices_per_node,
                            __global float *layer_weights,
                            // num_nodes
                            __global float *layer_biases) {
  const int node_idx = get_global_id(0);
  const float delta_j = layer_error[node_idx];
  const float learning_rate_times_delta_j = learning_rate * delta_j;

  for (int input_idx = 0; input_idx < indices_per_node; input_idx++) {
    const int gidx = indices_per_node * node_idx + input_idx;
    // Offset of the node, which we use to get its output.
    const int src_idx = layer_indices[gidx];
    const float x_ji = layer_values[src_idx];

    layer_weights[gidx] += learning_rate_times_delta_j * x_ji;
  }
  layer_biases[node_idx] += learning_rate_times_delta_j;
}
