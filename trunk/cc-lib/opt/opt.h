// Black-box optimization. This takes an arbitrary function R^N -> R,
// bounds on the inputs, and tries to find an input in bounds that
// minimizes the return value.
//
// This is just a simple wrapper around "BiteOpt"; see here:
// https://github.com/avaneev/biteopt

#ifndef _CC_LIB_OPT_H
#define _CC_LIB_OPT_H

#include <utility>
#include <vector>
#include <array>
#include <functional>

struct Opt {
  // Returns the parameter vector that produced the smallest
  // value, and the value of f on that vector.
  template<int N>
  static std::pair<std::array<double, N>, double>
  Minimize(const std::function<double(const std::array<double, N> &)> &f,
	   // Bounds must be finite.
	   const std::array<double, N> &lower_bound,
	   const std::array<double, N> &upper_bound,
	   // Approximately, the number of times to call f.
	   int iters,
	   int depth = 1,
	   int attempts = 10);

  // TODO: Should be possible to make a templated version that takes
  // f(x, y, z, ...)!

  // As above, but with n as a runtime value.
  static std::pair<std::vector<double>, double>
  Minimize(int n,
	   const std::function<double(const std::vector<double> &)> &f,
	   const std::vector<double> &lower_bound,
	   const std::vector<double> &upper_bound,
	   int iters,
	   int depth = 1,
	   int attempts = 10);

private:
  typedef double (*internal_func)(int N, const double* x,
				  void* func_data);

  static void internal_minimize(
      const int N, internal_func f, const void* data,
      const double* lb, const double* ub, double* x, double* minf,
      const int iter, const int M = 1, const int attc = 10);
};


// Template implementations follow.

template<int N>
std::pair<std::array<double, N>, double>
Opt::Minimize(
    const std::function<double(const std::array<double, N> &)> &f,
    // Bounds must be finite.
    const std::array<double, N> &lower_bound,
    const std::array<double, N> &upper_bound,
    // Approximately, the number of times to call f.
    int iters,
    int depth,
    int attempts) {
  static_assert(N > 0);

  auto wrap_f = [](int n_, const double *args, void* data) -> double {
      auto *f = (std::function<double(const std::array<double, N> &)> *)data;
      std::array<double, N> in;
      for (int i = 0; i < N; i++) in[i] = args[i];
      return (*f)(in);
    };
  std::array<double, N> out;
  double out_v = 0.0;
  Opt::internal_minimize(N, +wrap_f, &f,
			 lower_bound.data(), upper_bound.data(),
			 out.data(), &out_v, iters, depth, attempts);
  return {out, out_v};
}


#endif
