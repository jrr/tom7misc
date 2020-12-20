
// All memories are the size of the output layer.
__kernel void SetOutputError(__global const float *restrict actual_outputs,
                             __global const float *restrict expected,
                             __global float *restrict output_error) {
  const int k = get_global_id(0);
  // Here we are multiplying by the derivative.
  // Derivative is defined in terms of f(x) (the actual output),
  // since this is most efficient for sigmoid and works for relu.
  const float out_k = actual_outputs[k];
  // Note in some presentations this is out_k - expected_k.
  output_error[k] = DERIVATIVE(out_k) * (expected[k] - out_k);
}
