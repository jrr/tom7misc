
#ifndef __PRIMES_H
#define __PRIMES_H

struct Primes {
  /* return a random integer relatively prime to x (> 0) */
  static int relativeto(int x);

  /* table of the first nprimes primes */
  static const int nprimes;
  static const int primetable[];
};


#endif
