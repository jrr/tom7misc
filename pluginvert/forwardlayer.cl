
// A layer is simply defined by the values of the previous layer, and
// weights of the incoming edges. Because our layers are so big (as a
// consequence of representing image data), we don't store this as
// a dense vector (it would be like (3*2^16)^2 floats; ~150GB); instead
// each node has a sparse set of its inputs from the previous layer.
//
// We have to know how many indices each node uses, as a constant.

// We don't actually need to know the number of nodes within the kernel;
// the global id just tells us which node we work on. But the number
// of indices per node is needed to compute offsets.
__kernel void ForwardLayer(int indices_per_node,
                           // size num_nodes[layer]
                           __global const float *previous_layer_outputs,
                           // size num_nodes[layer + 1] * indices_per_node.
                           __global const int *indices,
                           // size num_nodes[layer + 1] * indices_per_node; parallel
                           // to the previous.
                           __global const float *weights,
                           // size num_nodes[layer + 1] (this layer).
                           __global const float *bias,
                           // size num_nodes[layer + 1].
                           __global float *output_values) {
  const int node_idx = get_global_id(0);

  // Start with bias.
  double potential = bias[node_idx];
  const __global float *my_weights = weights + (node_idx * indices_per_node);
  const __global int *my_indices = indices + (node_idx * indices_per_node);

  // Could itself be a kernel? Not sure what the right granularity of these is.
  for (int i = 0; i < indices_per_node; i++) {
    const float w = my_weights[i];
    const float v = previous_layer_outputs[my_indices[i]];
    potential += w * v;
  }
  output_values[node_idx] = 1.0 / (1.0 + exp(-potential));
}
