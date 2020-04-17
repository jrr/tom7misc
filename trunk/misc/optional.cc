// Another idea for "pattern-matching" decomposition of std::optional.
// Here, a for loop that is entered 0 or 1 times.

#include <optional>
#include <string>


namespace std {
template<class T>
class optional_iterator {
 public:
  optional_iterator(T *t) : t(t) {}
  optional_iterator &operator++(int dummy) {
    optional_iterator old(t);
    t = nullptr;
    return old;
  }

  optional_iterator &operator++() {
    t = nullptr;
    return *this;
  }
  
  T &operator*() {
    return *t;
  }

  const T &operator*() const {
    return *t;
  }

  bool operator!=(const optional_iterator &other) const {
    return t != other.t;
  }

 private:
  // nullptr = end
  T *t = nullptr;
};


template<class T>
optional_iterator<T> begin(std::optional<T> &o) {
  if (o.has_value()) return optional_iterator<T>(&*o);
  else return optional_iterator<T>(nullptr);
}

template<class T>
optional_iterator<T> end(std::optional<T> &o) {
  return optional_iterator<T>(nullptr);
}

// would need const_iterator and const versions of begin/end..

}  // std

int main(int argc, char **argv) {
  std::optional<std::string> foo; // = "yes";

  ++std::begin(foo) != std::end(foo);
#if 1
  for (const std::string &f : foo) {
    printf("%s\n", f.c_str());
  }
#endif
  
  return 0;
}

