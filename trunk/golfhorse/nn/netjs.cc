
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"
#include "../../cc-lib/util.h"

#include "network.h"

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

int ipow(int base, int exponent) {
  int res = 1;
  while (exponent--)
    res *= base;
  return res;
}

struct NNEncoder {
  // H is the number of symbols in the history.
  // B is the base of the number system.
  // W is the width of rationalized probabilities (denominator becomes
  // B^W). W=1 does not make much sense (everything would get discretized
  // to 1/B) unless B is larger than the actual number of symbols.
  NNEncoder(const Network &net, int H,
	    int nsymbols, int B, int W) :
    net(net), H(H), nsymbols(nsymbols), B(B), W(W) {
    // Note: Should be possible to adapt this to use some power of B
    // here, but we don't need that for our purposes since B is large.
    CHECK(nsymbols <= B);
  }

  void Encode(const vector<int> &symbols) {
    for (int x : symbols) {
      CHECK(x >= 0 && x < nsymbols) << x << " / " << nsymbols;
    }

    
  }

  // Returns a vector of pairs (happens to be in sorted order)
  // {symbol, mass} where each integral mass is > 0 and they
  // sum to B^W exactly.
  vector<pair<int, int>> Predict(const vector<int> &hist) {
    CHECK(hist.size() == H);
    // Use doubles since this is what we have in JS.
    StimulationD stim{net};

    vector<double> *input = &stim.values[0];
    CHECK_EQ(input->size(), nsymbols * H);
    for (int i = 0; i < H; i++) {
      const int s = hist[i];
      CHECK((nsymbols * i) + s < input->size());
      (*input)[nsymbols * i + s] = 1.0;
    }
    
    ForwardStimulationD(net, &stim);

    vector<pair<int, double>> output;
    output.reserve(nsymbols);
    CHECK_EQ(stim.values.back().size(), nsymbols);
    for (int i = 0; i < nsymbols; i++) {
      output.emplace_back(i, stim.values.back()[i]);
    }

    for (const auto &p : output)
      CHECK(!std::isnan(p.second));
    
    // In order to simplify discretizing the probabilities
    // later, we sort this in descending order. Unknown
    // whether sorting affects encoding? It might make
    // a difference?
    std::sort(output.begin(), output.end(),
	      [](const pair<int, double> &a,
		 const pair<int, double> &b) {
		// Stable in case of ties.
		if (b.second == a.second)
		  return a.first < b.first;

		// descending by probability
		return b.second > a.second;
	      });
    
    // Now, discretize the probabilities into units of 1/(B^W).
    // Some experimentation here is worthwhile, including
    // setting minimum probabilities.
    
    // Normalize all values so that they are in [0,1]. They
    // can have any value to start; negative is common...
    double minv = output[0].second, maxv = output[0].second;
    for (int i = 1; i < output.size(); i++) {
      minv = std::min(output[i].second, minv);
      maxv = std::max(output[i].second, maxv);
    }

    double norm = 1.0 / (maxv - minv);
    double sum = 1.0;
    for (int i = 0; i < output.size(); i++) {
      double d = output[i].second;
      d = (d - minv) * norm;
      sum += d;
      output[i].second = d;
    }

    // Now all of the values are in [0,1] and sum to sum. Assign a
    // discrete probability to each, and make sure that none is zero,
    // because we need to be able to encode any symbol even if we
    // really messed up the prediction! We want an easy procedure that
    // we can replicate in javascript, so we go from lowest
    // probability (end of array) to highest, and round down (!) to
    // an integer (but a minimum of 1). The remaining mass is always
    // assigned to the largest bucket, which comes last. This means
    // that rounding error is "stolen" from this one.
    int mass_left = ipow(B, W);
    double total_mass = mass_left;
    
    // TODO: Experiment with rounding to the closest, etc. Need to
    // keep this simple though, because it has to be replicated in JS.
    vector<pair<int, int>> discrete_out;
    discrete_out.resize(nsymbols);
    for (int i = output.size() - 1; i > 0; i--) {
      CHECK(mass_left > 0);
      int sym = output[i].first;
      double r = (output[i].second / sum) * total_mass;
      if (r < 1.0) {
	mass_left--;
	discrete_out[i] = {sym, 1};
      } else {
	int d = (int)r;
	CHECK(d >= 1);
	mass_left -= d;
	discrete_out[i] = {sym, d};
      }
    }
    // Always assign the remainder to the biggest (first) bucket.
    CHECK(mass_left > 0);
    discrete_out[0] = {output[0].first, mass_left};
    mass_left = 0;

    return discrete_out;
  }
  
  const Network &net;
  const int H, nsymbols, B, W;
};



bool IsDense(int num_src_nodes,
	     int num_dst_nodes,
	     const Network::Layer &layer) {
  CHECK(layer.indices.size() ==
	num_dst_nodes * layer.indices_per_node);
  CHECK(layer.indices_per_node == num_src_nodes);

  // They should also be in sorted order. We could fix this
  // if not, the Network is expected to be constructed that
  // way anyway.
  for (int n = 0; n < num_dst_nodes; n++) {
    for (int i = 0; i < layer.indices_per_node; i++) {
      if (i != layer.indices[n * layer.indices_per_node + i]) return false;
    }
  }
  return true;
}

static constexpr bool VERBOSE = true;

int main(int argc, char **argv) {
  std::unique_ptr<Network> net{Network::ReadNetworkBinary("net.val")};
  CHECK(net.get());
  
  // The network is dense, so we don't need the indices (nor inverted
  // indices.)
  for (int i = 0; i < net->layers.size(); i++) {
    const Network::Layer &layer = net->layers[i];
    CHECK(IsDense(net->num_nodes[i], net->num_nodes[i + 1], layer));
  }

  int64 num_floats = 0LL;
  
  // TODO...
  // In JavaScript, the main thing we care about is the compactness
  // of the encoding. For starts, just emit the decimal floats.
  string js = "N={n:[";
  // n=num_nodes array.
  for (int i = 0; i < net->num_nodes.size(); i++) {
    if (i != 0) js += ",";
    js += StringPrintf("%d", net->num_nodes[i]);
  }
  js += "],";
  if (VERBOSE) js += "\n";
  // Now the layers. indices_per_node is always the size of the
  // previous layer. Transfer function is always leaky_relu.
  // indices are the identity. So we have
  // num_nodes[l + 1] floats (biases), then
  // num_nodes[l + 1] * num_nodes[l] floats (weights)
  js += "l:[";
  for (int layer = 0; layer < net->num_layers; layer++) {
    js += "[";
    int ipn = net->layers[layer].indices_per_node;
    if (VERBOSE) js += StringPrintf("\n/* *** layer %d *** */\n", layer);
    if (VERBOSE) js += "\n /* biases */\n  ";
    for (const float b : net->layers[layer].biases)
      js += StringPrintf("%.9g,", b);
    num_floats += net->layers[layer].biases.size();
    if (VERBOSE) js += "\n /* weights */";
    for (int n = 0; n < net->num_nodes[layer + 1]; n++) {
      if (VERBOSE) js += StringPrintf("\n   /* node %d */\n   ", n);
      string ws;
      for (int i = 0; i < ipn; i++) {
	ws += StringPrintf("%.9g,",
			   net->layers[layer].weights[n * ipn + i]);
      }
      js += ws;
    }
    num_floats += net->num_nodes[layer + 1] * ipn;
    js += "],";
  }
  js += "]}\n";

  // printf("%s", js.c_str());


  // Use the network to generate probability mass functions, and
  // feed those to arithmetic encoder.

  
  
  
  // Obviously this shouldn't be part of the final output, but it
  // is good for playin' around.
  string dict = Util::ReadFile("../wordlist.asc");
  
  printf("Original dict size: %lld\n"
	 "Net: %lld floats. *2 = %lld, *3 = %lld, *4 = %lld.\n",
	 (int64)dict.size(),
	 num_floats, num_floats * 2LL, num_floats * 3LL,
	 num_floats * 4LL);

  dict = Util::Replace(dict, "\n", "\\n");
  
  string netcode = Util::ReadFile("netcode.js");
  // TODO: if not verbose, strip lines starting with // and
  // trailing whitespace!

  string evalcode = Util::ReadFile("evalcode.js");
  
  FILE *f = fopen("overfit.js", "wb");
  fprintf(f, "%s", js.c_str());
  fprintf(f, "let dict='%s'\n", dict.c_str());
  fprintf(f, "%s", netcode.c_str());
  fprintf(f, "%s", evalcode.c_str());
  // XXX need diffs, etc.
  fclose(f);
  
  return 0;
}

