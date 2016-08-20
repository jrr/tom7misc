
#include "generator.h"
#include "util.h"
#include "prime.h"

Generator::Generator(unsigned int s) : size(s) {
  /* generate a random prime c that does
     not divide size. */
  c = Prime::relativeto(s);

  /* pick any random starting point */
  x = ((unsigned)util::random()) % size;

  /* period is equal to size */
  left = (int)size;

  /*
    printf("Generator: start %d, c = %d, size = %d\n",
    x, c, size); */
}

Generator::Generator(unsigned int s, void *ignored) 
  : size(s), c(1), x(0) {
  left = (int)size;
}

void Generator::next() {
  x = (a * x + c) % size;
  left--;
  if (left < 0) left = 0;
  /* printf("next item is %d\n", x); */
}

unsigned int Generator::item() const {
  return x;
}

bool Generator::anyleft() const {
  return !!left;
}
