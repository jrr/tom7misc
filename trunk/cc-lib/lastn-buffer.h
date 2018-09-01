
#ifndef __LASTN_BUFFER_H
#define __LASTN_BUFFER_H

#include <vector>

// LastNBuffer<T> is an efficient fixed-size (N elements) buffer that
// stores (only) the last N elements of type T pushed into it. Pushing
// is constant time and accessing any element by index is also
// constant time. (It is implemented as a circular vector of size N.)
//
// T is expected to have value semantics.

template<class T>
struct LastNBuffer {
  // The buffer begins containing n copies of default_value.
  LastNBuffer(int n, T default_value);

  // If the front of the array (element 0) is at the left,
  // rotate the contents to the left so that element zero
  // becomes the final element, element 1 becomes the 0
  // element, etc.
  void RotateLeft();
  // Inverse of the above.
  void RotateRight();

  // Last element. Element zero falls off.
  void push_back(const T &t);
  // void push_back(T &&t);   // TODO
  // Element zero. An element falls off the back.
  void push_front(const T &t);
  // void push_front(T &&t); // TODO

  int size() const { return n; }
  
  T &operator [](int i) {
    return data[Wrap(zero + i)];
  }
  const T &operator [](int i) const {
    return data[Wrap(zero + i)];
  }

  template<class F>
  void App(F f) const {
    // PERF don't need to compute indices so many times
    for (int i = 0; i < n; i++) {
      f((*this)[i]);
    }
  }

  
 private:
  // Like idx % n, but only works when idx is in [0, 2 * n - 1]. This
  // is sufficient because "zero" is always in [0, n - 1].
  inline int Wrap(int idx) const {
    return (idx >= n) ? idx - n : idx;
  }
  // Always in [0, n - 1].
  int zero = 0;
  // n == data.size()
  const int n = 0;
  std::vector<T> data;
};

// Template implementations follow.
template<class T>
LastNBuffer<T>::LastNBuffer(int n, T default_value) :
  zero(0), n(n), data(n, default_value) {}

template<class T>
void LastNBuffer<T>::RotateLeft() {
  zero = Wrap(zero + 1);
}

template<class T>
void LastNBuffer<T>::RotateRight() {
  zero--;
  if (zero < 0) zero += n;
}

template<class T>
void LastNBuffer<T>::push_back(const T &t) {
  data[zero] = t;
  RotateLeft();
}

template<class T>
void LastNBuffer<T>::push_front(const T &t) {
  RotateRight();
  data[zero] = t;
}

#endif
