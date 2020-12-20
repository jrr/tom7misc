
// Port of BackwardsError in C++ code.
// Propagates errors from the destination to the source.

// Expects the following defines:
// DST_INDICES_PER_NODE, an integer giving the number of indices
//   that each node in the destination layer has. This is a constant
//   with the hopes that the integer division can be done with tricks.
//
// DST_NUM_NODES, an integer.
//
// DERIVATIVE, the derivative of the transfer function, given in
//   terms of the function's output.

// For when the destination layer is sparse.
__kernel void BackwardLayerSparse(
                  // Size src_num_nodes.
                  __global const uint *restrict inverted_indices_start,
                  __global const uint *restrict inverted_indices_length,
                  // Size dst_num_nodes * dst_indices_per_node.
                  __global const int *restrict inverted_indices,
                  // Size dst_num_nodes * dst_indices_per_node.
                  __global const float *restrict dst_weights,
                  // Size src_num_nodes.
                  __global const float *restrict src_output,
                  // Size dst_num_nodes.
                  __global const float *restrict dst_error,
                  // Size src_num_nodes; finally where we write:
                  __global float *restrict src_error) {
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
    const int dst_node_idx = gidx / DST_INDICES_PER_NODE;

    // fma() seems slightly faster.
    // weighted_error_sum += dst_weights[gidx] * dst_error[dst_node_idx];
    weighted_error_sum = fma(dst_weights[gidx], dst_error[dst_node_idx],
                             weighted_error_sum);
  }

  src_error[h] = DERIVATIVE(out_h) * weighted_error_sum;
}

// For when the destination layer is dense.
__kernel void BackwardLayerDense(
                  // Size src_num_nodes.
                  // XXX unnecessary
                  __global const uint *restrict inverted_indices_start,
                  // XXX unnecessary
                  __global const uint *restrict inverted_indices_length,
                  // Size dst_num_nodes * dst_indices_per_node.
                  // XXX unnecessary
                  __global const int *restrict inverted_indices,
                  // Size dst_num_nodes * dst_indices_per_node.
                  __global const float *restrict dst_weights,
                  // Size src_num_nodes.
                  __global const float *restrict src_output,
                  // Size dst_num_nodes.
                  __global const float *restrict dst_error,
                  // Size src_num_nodes; finally where we write:
                  __global float *restrict src_error) {
  // h in 0..src_num_nodes
  const int h = get_global_id(0);
  const float out_h = src_output[h];

  // The destination layer is dense, so each node there reads from
  // the source output at the same index. The offset is this node's
  // index (h), the stride is DST_INDICES_PER_NODE, and there
  // are DST_NUM_NODES such outputs.

  // Unpack inverted index for this node, so that we can loop over all of
  // the places its output is sent.
  // const uint start = inverted_indices_start[h];
  // const uint length = inverted_indices_length[h];

  // The error for a hidden node is the sum of all the errors for
  // the next layer, but modulated by the weight of the edge.
  float weighted_error_sum = 0.0f;
  for (int i = 0; i < DST_NUM_NODES; i++) {
    const int gidx = h + DST_INDICES_PER_NODE * i;

    // fma() seems slightly faster.
    // weighted_error_sum += dst_weights[gidx] * dst_error[dst_node_idx];
    weighted_error_sum = fma(dst_weights[gidx], dst_error[i],
                             weighted_error_sum);
  }

  src_error[h] = DERIVATIVE(out_h) * weighted_error_sum;
}
