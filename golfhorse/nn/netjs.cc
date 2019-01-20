
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <deque>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"
#include "../../cc-lib/util.h"

#include "timer.h"
#include "network.h"
#include "arith.h"

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

#define VERBOSE false

struct NNEncoder : public ArithEncoder {
  // H is the number of symbols in the history.
  // B is the base of the number system.
  // W is the width of rationalized probabilities (denominator becomes
  // B^W). W=1 does not make much sense (everything would get discretized
  // to 1/B) unless B is larger than the actual number of symbols.
  NNEncoder(const Network &net, int H,
	    int nsymbols, int B, int W) :
    ArithEncoder(H, nsymbols, B, W), net(net) {
  }

  // Returns a vector of pairs (happens to be in sorted order)
  // {symbol, mass} where each integral mass is > 0 and they
  // sum to B^W exactly.
  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    CHECK(hist.size() == H) << hist.size() << " / " << H;
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

    return Discretize(stim.values.back());
  }
  
  const Network &net;
};

int ToSymbol(char c) {
  if (c >= 'a' && c <= 'z') return c - 'a';
  if (c == '\n') return 26;
  CHECK(false) << "Bad char " << c;
  return 0;
}

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

// For debugging, use a flat model. No history, base 10.
#if 0
struct ExampleEncoder : public ArithEncoder {
  ExampleEncoder() : ArithEncoder(0, 3, 3, 3) {}

  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    return {{0, 1},
	    {1, 25},
	    {2, 1}};
  }
};
#endif

vector<int> MakeSymbols(const string &str) {
  vector<int> syms;
  for (char s : str) {
    syms.push_back(ToSymbol(s));
  }
  return syms;
}


int main(int argc, char **argv) {
#if 0
  // Test some math.
  printf("\n");
  Big a(10, 123, 3);
  Big b(10, 10, 1);
  Big c = PlusSameDenom(a, a);
  a.Shift(1);
  b.Shift(3);
  a.Unzero();
  b.Unzero();
  Big d(10, 199, 3);
  Big e = MinusSameDenom(c, d);
  Big f = Scale(e, 999, 3);
  e.Shift(3);
  printf("a: %s\n", a.ToString().c_str());
  printf("b: %s\n", b.ToString().c_str());
  printf("c: %s\n", c.ToString().c_str());
  printf("d: %s\n", d.ToString().c_str());
  printf("e: %s\n", e.ToString().c_str());
  printf("f: %s\n", f.ToString().c_str());
  CHECK(LessEq(a, b));
  CHECK(Less(a, b));
  CHECK(LessEq(a, a));
  
  CHECK(Less(f, e));
  CHECK(LessEq(f, e));
  CHECK(!Less(e, f));
  CHECK(!LessEq(e, f));
#endif

#if 0
  ExampleEncoder example;
  example.Encode(// {2, 1, 0}
      MakeSymbols("acababaccabbb"));

  return 0;
#endif
  
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

  // Obviously this shouldn't be part of the final output, but it
  // is good for playin' around.
  string dict = Util::ReadFile("../wordlist.asc");


  // Use the network to generate probability mass functions, and
  // feed those to arithmetic encoder.

  NNEncoder encoder{*net, 5, 27, 124, 1};

  dict.resize(1000);
  vector<int> symbols = // MakeSymbols("antidisestablishmentarianism");
    MakeSymbols(dict);
  encoder.Encode(symbols);

  return 0;
  
  
  
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

