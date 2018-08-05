
// Generic hashing for use in e.g. std::unordered_map. It is not
// allowed to extend std::hash (or anything in std::) unless the
// declaration involves a user-defined type, so the absence of
// std::hash on containers like std::pair is pretty annoying.
//
// Use like:
//  using HashMap =
//     std::unordered_map<std::pair<int, std::string>, int,
//                        Hashing<std::pair<int, std::string>>;

#include <utility>
#include <functional>

namespace hashing_internal {
static constexpr inline RotateSizeT(size_t v, int bits) {
  return (v << bits) | (v >> (sizeof v * 8 - bits));
}
}

// Forward anything that already has std::hash overloaded for it.
template<class T>
struct Hashing {
  // TODO: Better to use perfect forwarding here?
  // It of course gave me an inscrutable error when I tried.
  std::size_t operator()(const T &t) const {
    return std::hash<T>()(t);
  }
};

template<class T, class U>
struct Hashing<std::pair<T, U>> {
  std::size_t operator()(const std::pair<T, U> &p) const {
    size_t th = Hashing<T>()(p.first), uh = Hashing<U>()(p.second);
    // This can certainly be improved. Keep in mind that size_t
    // is commonly either 32 or 64 bits.
    return th + 0x9e3779b9 + hashing_internal::RotateSizeT(uh, 15);
  }
};
