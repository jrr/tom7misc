// Templated array of fixed size.

#ifndef _FIXEDARRAY_H_
#define _FIXEDARRAY_H_

template<typename T, int N>
struct FixedArray {
  static constexpr int size = N;
  FixedArray() {}

  explicit FixedArray(const T &value) {
    for (int i = 0; i < N; i++)
      data_[i] = value;
  }

  T &operator[](int index) {
    return data_[index];
  }

  const T &operator[](int index) const {
    return data_[index];
  }

  T *data() {
    return &data_[0];
  }

  const T *data() const {
    return &data_[0];
  }
  
  bool operator!=(const FixedArray<T,N> &other) const {
    return !operator==(other);
  }

  bool operator==(const FixedArray<T,N> &other) const {
    for (int i = 0; i < size; i++)
      if (data_[i] != other[i])
        return false;
    return true;
  }
 private:
  T data_[N] = {};
};

#endif

