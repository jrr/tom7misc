
#include "edit-distance.h"

#include <algorithm>
#include <string>

using namespace std;

#define score(c, d) (((c)==(d))?0:1)
#define gapscore 1

// Weirdly, this approach is fastest in benchmarks.
#define int_min(a, b) (((a)<(b))?(a):(b))
#define int_min_3(a, b, c) (((a)<(b))?int_min(a,c):int_min(b,c))

/*
inline int int_min_3(int a, int b, int c) {
  return std::min(std::min(a, b), c);
}
*/

#define ba(x, y) (a[(y) * n1 + (x)])

#define aft(i) a[(parity & (n1 + 1)) + (i)]
#define fore(i) a[((~parity) & (n1 + 1)) + (i)]

#define swaplines() parity = ~parity

int EditDistance::Distance(const string &s1, const string &s2) {
  const int n1 = s1.size();
  const int n2 = s2.size();
  
  int *a = (int *) malloc((n1 + 1) * 2 * sizeof(int));

  /* parity will always be either 0 or 0xFFFFFFFF */
  int parity = 0;

  // Initialize base cases.
  for (int i = 0; i <= n1; i++)
    aft(i) = gapscore * i;

  // Now, compute each row from the previous row.
  for (int y = 1; y <= n2; y ++) {
    // We know what the first column of every row is.
    int last = gapscore * y;
    fore(0) = last;

    for (int x = 1; x <= n1; x ++) {
      int xi = x - 1;
      int yi = y - 1;

      int diag = aft(x - 1) + score(s1[xi], s2[yi]);

      int up   = aft(x) + gapscore;
      int left = last + gapscore;

      last = int_min_3(up, left, diag);

      /*
      int ul = std::min(aft(x), last) + gapscore;
      last = std::min(ul, diag);
      */
      
      fore(x) = last;
    }

    // Move fore to aft in preparation for next line.
    swaplines();
  }

  // At this point we swapped our final line into aft, so we want
  // the last cell of aft.

  int ans = aft(n1);
  free(a);
  return ans;
}

// The following is a somewhat literal port of a JavaScript implementation
// of Ukkonen's algorithm ("Algorithms for approximate string matching")
// found at https://github.com/sunesimonsen/ukkonen
//
// Copyright (c) 2017 Sune Simonsen <sune@we-knowhow.dk>
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the 'Software'), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

int EditDistance::Ukkonen(const string &s1, const string &s2, int threshold) {
  const char *a = s1.c_str();
  const char *b = s2.c_str();
  int na = s1.size();
  int nb = s2.size();
  // Put in normal form (a is the shorter one).
  if (s1.length() > s2.length()) {
    const char *t = a;
    a = b;
    b = t;
    int nt = na;
    na = nb;
    nb = nt;
  }

  while (na > 0 && nb > 0 && *a == *b) {
    a++;
    b++;
    na--;
    nb--;
  }

  if (na == 0) return std::min(nb, threshold);
  
  while (na > 0 && nb > 0 && a[na - 1] == b[nb - 1]) {
    na--;
    nb--;
  }

  if (na == 0) return std::min(nb, threshold);

  // Can't possibly have an edit distance larger than the longer string.
  if (nb < threshold)
    threshold = nb;

  const int nd = nb - na;

  // Cost must be at least the difference in length between the two strings,
  // so if this is more than the threshold, the answer is the threshold.
  // XXX could be <= ?
  if (threshold < nd) {
    return threshold;
  }

  // floor(min(threshold, aLen) / 2)) + 2
  const int ZERO_K = ((na < threshold ? na : threshold) >> 1) + 2;

  const int array_length = nd + ZERO_K * 2 + 2;

  int *current_row = (int *)malloc(array_length * sizeof(int));
  int *next_row = (int *)malloc(array_length * sizeof(int));  

  for (int i = 0; i < array_length; i++) {
    current_row[i] = -1;
    next_row[i] = -1;
  }

  int i = 0;
  const int condition_row = nd + ZERO_K;
  const int end_max = condition_row << 1;
  do {
    i++;

    {
      int *tmp = current_row;
      current_row = next_row;
      next_row = tmp;
    }
      
    int start = 0;
    int next_cell = 0;

    if (i <= ZERO_K) {
      start = -i + 1;
      next_cell = i - 2;
    } else {
      start = i - (ZERO_K << 1) + 1;
      next_cell = current_row[ZERO_K + start];
    }

    int end = 0;
    if (i <= condition_row) {
      end = i;
      next_row[ZERO_K + i] = -1;
    } else {
      end = end_max - i;
    }

    int current_cell = -1;
    for (int k = start, row_index = start + ZERO_K; k < end; k++, row_index++) {
      int previous_cell = current_cell;
      current_cell = next_cell;
      next_cell = current_row[row_index + 1];

      // max(t, previous_cell, next_cell + 1)
      int t = current_cell + 1;
      t = t < previous_cell ? previous_cell : t;
      t = t < next_cell + 1 ? next_cell + 1 : t;

      while (t < na && t + k < nb && a[t] == b[t + k]) {
        t++;
      }

      next_row[row_index] = t;
    }
  } while (next_row[condition_row] < na && i <= threshold);

  free(current_row);
  free(next_row);
  return i - 1;
}

// (end copyright)
