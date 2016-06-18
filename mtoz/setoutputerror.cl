
// All memories are the size of the output layer.
__kernel void SetOutputError(__global const float *actual_outputs,
			     __global const float *expected,
			     __global float *output_error) {
  const int k = get_global_id(0);
  // Here we want to multiply by the derivative, sigma'(input),
  // which is sigma(input) * (1.0 - sigma(input)), and we already have
  // sigma(input) -- it's the output.
  const float out_k = actual_outputs[k];
  // Note in some presentations this is out_k - expected_k.
  output_error[k] = out_k * (1.0 - out_k) * (expected[k] - out_k);
}
