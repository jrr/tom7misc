
#ifndef __HUFFMAN_H
#define __HUFFMAN_H

#include <utility>
#include <vector>
#include <map>

using namespace std;

// TODO: If good, move to cc-lib...

// Compute a Huffman tree.
struct Huffman {
  Huffman() {};
  struct Node {
    Node(Node *l, Node *r) : left(l), right(r) {}
    explicit Node(int s) : sym(s) {}
    // Always both present (internal) or absent (leaf).
    Node *left = nullptr, *right = nullptr;
    // Symbol
    int sym = -1;
  };

  // Symbol must be nonnegative.
  void AddSymbol(int sym, int freq) {
    CHECK(sym >= 0);
    nodes.insert(make_pair(freq, new Node(sym)));
    max_sym = std::max(max_sym, sym);
  }

  void MakeTree() {
    while (nodes.size() > 1) {
      int af, bf;
      Node *a, *b;
      std::tie(af, a) = PopFront();
      std::tie(bf, b) = PopFront();
      nodes.insert(make_pair(af + bf, new Node(a, b)));
    }
  }

  // Outputs the codes. Empty vector means no code assigned.
  // Assumes the input symbols are dense enough for this to
  // be a reasonable representation.
  vector<vector<bool>> MakeCodes() {
    vector<vector<bool>> out;
    out.resize(max_sym + 1);
    vector<bool> code;
    MakeCodesRec(&out, &code, nodes.begin()->second);
    return out;
  }
  
  void PrintCodes() {
    CHECK(nodes.size() == 1);
    PrintCodesRec("", nodes.begin()->second);
  }

  void PrintTree() {
    for (const auto &p : nodes) {
      printf("[Freq %d]\n", p.first);
      PrintTreeRec(2, p.second);
    }
  }
  
private:
  void MakeCodesRec(vector<vector<bool>> *out,
		    vector<bool> *code,
		    Node *n) {
    if (n->left == nullptr) {
      (*out)[n->sym] = *code;
    } else {
      code->push_back(false);
      MakeCodesRec(out, code, n->left);
      code->pop_back();
      code->push_back(true);
      MakeCodesRec(out, code, n->right);
      code->pop_back();
    }
  }

  void PrintCodesRec(string code, Node *n) {
    if (n->left == nullptr) {
      printf("%s '%d'\n", code.c_str(), n->sym);
    } else {
      PrintCodesRec(code + "0", n->left);
      PrintCodesRec(code + "1", n->right);
    }
  }

  void PrintTreeRec(int depth, Node *n) {
    for (int i = 0; i < depth; i++) printf(" ");
    if (n->left == nullptr) {
      printf("'%d'\n", n->sym);
    } else {
      PrintTreeRec(depth + 2, n->left);
      PrintTreeRec(depth + 2, n->right);
    }
  }
  
  std::pair<int, Node *> PopFront() {
    CHECK(!nodes.empty());
    auto ret = *nodes.begin();
    nodes.erase(nodes.begin());
    return ret;
  }
  std::multimap<int, Node *> nodes;
  int max_sym = 0;
};

#endif
