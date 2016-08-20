
#ifndef __PRIME_H
#define __PRIME_H

struct Prime {
  /* return a random integer relatively prime to x (> 0) */
  static int relativeto(int x);

  /* table of the first nprimes primes */
  static const int nprimes;
  static const int primetable[];
};


#endif
