
#ifndef __BIGINT_H
#define __BIGINT_H

#include "bigz.h"
#include "bign.h"
#include "bigq.h"

#include <cstdint>
#include <string>

struct BigInt {
  BigInt() : BigInt(0LL) {}
  explicit BigInt(int64_t n);

  // Value semantics with linear-time copies (like std::vector).
  BigInt(const BigInt &other);
  BigInt &operator =(const BigInt &other);
  BigInt &operator =(BigInt &&other);

  ~BigInt();

  // In base 10.
  std::string ToString(int base = 10) const;

  void Swap(BigInt *other);
  
private:
  // BigZ is a pointer to a bigz struct, which is the
  // header followed by digits.
  BigZ bigz;
};

BigInt::BigInt(int64_t n) : bigz(BzFromInteger(n)) { }

BigInt::BigInt(const BigInt &other) : bigz(BzCopy(other.bigz)) { }
BigInt &BigInt::operator =(const BigInt &other) {
  // Self-assignment does nothing.
  if (this == &other) return *this;
  BzFree(bigz);
  bigz = BzCopy(other.bigz);
  return *this;
}
BigInt &BigInt::operator =(BigInt &&other) {
  // We don't care how we leave other, but it needs to be valid (e.g. for
  // the destructor). Swap is a good way to do this.
  Swap(&other);
  return *this;
}

BigInt::~BigInt() { BzFree(bigz); }

void BigInt::Swap(BigInt *other) {
  BigZ tmp = bigz;
  bigz = other->bigz;
  other->bigz = tmp;
}

std::string BigInt::ToString(int base) const {
  // Allocates a buffer.
  // Third argument forces a + sign for positive; not used here.
  BzChar *buf = BzToString(bigz, base, 0);
  std::string ret{buf};
  BzFreeString(buf);
  return ret;
}

#endif
