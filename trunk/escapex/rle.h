/* RLE compression of integer arrays.
   (Note: Not the same format as cc-lib's RLE.)

   The first byte says how many bits are used to represent
   integers.

   We can use any number of bits (0..32), with the following
   encoding:

   high bit 1: byte & 0b00111111 gives the bit count, which
               must be <= 32.

   high bit 0: then bit count is byte * 8.
     (this is for backwards compatibility with
      the old byte-based scheme)

   If the bit count is zero, then only the integer zero can
   be represented, and it is represented by the empty bit
   string.

   If the first and second highest bits are set, then the first byte
   is followed by 5 bits, which is the value called 'framebits'.
   (Otherwise, framebits is assumed to be 8.) This value gives the
   bit length of frame headers. It must be at least 1.

   Then, we have repeating frames to generate the expected
   number of ints.

   A frame is:

      framebits bits representing a run count [1,2^framebits), followed
      by an integer (written with some number of bits, depending on the
      count above) which means 'count' copies of the integer. The
      idea is that we can compress runs of equal values.

      or

      framebits of 0, followed by framebits bits giving an anti-run
      count [1,2^framebits), then 'count' integers each encoded as above. The
      idea here is to avoid counts of '1' when the values are
      continually different.
*/

#ifndef __ESCAPE_RLE_H
#define __ESCAPE_RLE_H

#include <string>

// XXX Since this deals with bits, we should specify the bit width of
// ints being encoded (or template).

// XXX write tests!

struct EscapeRLE {
  /* idx a starting position (measured in bytes) in 'string' for the
     rle-encoded data, which is modified to point to the next byte after
     the data if the call is successful. n is the number of integers we
     sould expect out.

     Returns a malloc-allocated array, which becomes owned by the caller.
  */
  static int *Decode(const std::string &s, unsigned int &idx_bytes, int n);

  /* encode n ints in 'a', and return it as a string.
     This uses a greedy strategy that is not optimal.
  */
  static std::string Encode(int n, const int *a);

 private:
  EscapeRLE() = delete;
};

#endif
