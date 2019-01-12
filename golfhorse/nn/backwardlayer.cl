
// Port of BackwardsError in C++ code.

__kernel void BackwardLayer(int dst_indices_per_node,
                            // Size src_num_nodes.
                            __global const uint *inverted_indices_start,
                            __global const uint *inverted_indices_length,
                            // Size dst_num_nodes * dst_indices_per_node.
                            __global const int *inverted_indices,
                            // Size dst_num_nodes * dst_indices_per_node.
                            __global const float *dst_weights,
                            // Size src_num_nodes.
                            __global const float *src_output,
                            // Size dst_num_nodes.
                            __global const float *dst_error,
                            // Size src_num_nodes; finally where we write:
                            __global float *src_error) {
  const int h = get_global_id(0);
  const float out_h = src_output[h];
  // Unpack inverted index for this node, so that we can loop over all of
  // the places its output is sent.
  const uint start = inverted_indices_start[h];
  const uint length = inverted_indices_length[h];

  // The error for a hidden node is the sum of all the errors for
  // the next layer, but modulated by the weight of the edge.
  float weighted_error_sum = 0.0f;
  for (int i = start; i < start + length; i++) {
    const int gidx = inverted_indices[i];
    // Compute from the index which destination node it belongs to.
    // PERF: Would be nice to avoid this integer division somehow.
    const int dst_node_idx = gidx / dst_indices_per_node;
    // PERF fma()?
    weighted_error_sum += dst_weights[gidx] * dst_error[dst_node_idx];
  }

  src_error[h] = DERIVATIVE(out_h) * weighted_error_sum;
}
