
// Sets the error on the output layer, which we can then propagate back up.
// Error distance is always linear, but this allows for remapping the values
// before computing that error. Define REMAP(i, x), which takes the node index
// and float value (either actual or expected), and returns a float.

// All memories are the size of the output layer.
__kernel void SetOutputError(__global const float *restrict actual_outputs,
                             __global const float *restrict expected,
                             __global float *restrict output_error) {
  const int k = get_global_id(0);

  const float out_k = actual_outputs[k];
  const float expected_k = expected[k];

  // Remapped values.
  const float rout_k = REMAP(k, out_k);
  const float rexpected_k = REMAP(k, expected_k);

  // Here we are multiplying by the derivative.
  // Derivative is defined in terms of f(x) (the actual output),
  // since this is most efficient for sigmoid and works for relu.

  // Note in some presentations this is out_k - expected_k.
  output_error[k] = DERIVATIVE(rout_k) * (rexpected_k - rout_k);
}
