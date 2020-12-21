
// A layer is simply defined by the values of the previous layer, and
// weights of the incoming edges. Because our layers are so big (as a
// consequence of representing image data), we don't store this as
// a dense vector (it would be like (3*2^16)^2 floats; ~150GB); instead
// each node has a sparse set of its inputs from the previous layer.
//
// We have to know how many indices each node uses, as a constant.

// Expects the following defines:

// FORWARD, the transfer function.
// INDICES_PER_NODE, an integer giving the number of output indices per
//   node.

// We don't actually need to know the number of nodes within the kernel;
// the global id just tells us which node we work on. But the number
// of indices per node is needed to compute offsets.
__kernel void ForwardLayerSparse(
                // size num_nodes[layer]
                __global const float *restrict previous_layer_outputs,
                // size num_nodes[layer + 1] * INDICES_PER_NODE.
                __global const int *restrict indices,
                // size num_nodes[layer + 1] * INDICES_PER_NODE; parallel
                // to the previous.
                __global const float *restrict weights,
                // size num_nodes[layer + 1] (this layer).
                __global const float *restrict bias,
                // size num_nodes[layer + 1].
                __global float *restrict output_values) {
  const int node_idx = get_global_id(0);

  // Start with bias.
  // PERF: float should obviously be faster, but I briefly got better
  // results with double?
  float potential = bias[node_idx];
  const __global float *my_weights = weights + (node_idx * INDICES_PER_NODE);
  const __global int *my_indices = indices + (node_idx * INDICES_PER_NODE);

  // Could itself be a kernel? Not sure what the right granularity of these is.
  for (int i = 0; i < INDICES_PER_NODE; i++) {
    const float w = my_weights[i];
    const float v = previous_layer_outputs[my_indices[i]];
    // potential += w * v;
    potential = fma(w, v, potential);
  }
  output_values[node_idx] = FORWARD(potential);
}

// Dense version. Here we can read the indices in order without any
// indirection, which is a lot faster.
__kernel void ForwardLayerDense(
                // size num_nodes[layer]
                __global const float *restrict previous_layer_outputs,
                // size num_nodes[layer + 1] * INDICES_PER_NODE; parallel
                // to the previous.
                __global const float *restrict weights,
                // size num_nodes[layer + 1] (this layer).
                __global const float *restrict bias,
                // size num_nodes[layer + 1].
                __global float *restrict output_values) {
  const int node_idx = get_global_id(0);

  // Start with bias.
  // PERF: float should obviously be faster, but I briefly got better
  // results with double?
  float potential = bias[node_idx];
  const __global float *my_weights = weights + (node_idx * INDICES_PER_NODE);

  // Could itself be a kernel? Not sure what the right granularity of these is.
  for (int i = 0; i < INDICES_PER_NODE; i++) {
    const float w = my_weights[i];
    const float v = previous_layer_outputs[i];
    // potential += w * v;
    potential = fma(w, v, potential);
  }
  output_values[node_idx] = FORWARD(potential);
}
