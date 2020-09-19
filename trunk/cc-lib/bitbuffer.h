#include <string>

/* treats strings as buffers of bits
   TODO: use cstdint types, possibly just rewrite
 */
struct BitBuffer {
  /* read n bits from the string s from bit offset idx.
     (high bits first)
     write that to output and return true.
     if an error occurs (such as going beyond the end of the string),
     then return false, perhaps destroying idx and output */
  static bool nbits(const std::string &s, int n, int &idx, unsigned int &output);

  /* create a new empty bit buffer */
  bitbuffer() : data(0), size(0), bits(0) { }

  /* appends bits to the bit buffer */
  void writebits(int width, unsigned int thebits);

  /* get the contents of the buffer as a string,
     padded at the end if necessary */
  string getstring();

  /* give the number of bytes needed to store n bits */
  static int ceil(int bits);


  ~BitBuffer();

  private:
  unsigned char * data;
  int size;
  int bits;
};
