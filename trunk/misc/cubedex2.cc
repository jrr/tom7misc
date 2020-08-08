
#include <stdio.h>
#include <vector>
#include <cstdint>

using namespace std;
using uint8 = uint8_t;

int main(int argc, char **argv) {
  vector<uint8> nums = {
    0b1000001,
    0b1000110,
    0b1010100,
    0b1000101,
    0b1010010,
    0b0100000,
    0b1000010,
    0b0100000,
    0b1001100,
    0b1000101,
    0b1010100,
    0b1010100,
    0b1000101,
    0b1010010,
    0b0100000,
    0b1000001,
    0b1000110,
    0b1010100,
    0b1000101,
    0b1010010,
  };

  for (int i : nums) {
    char c = i; // 'a' + i;
    printf("%c", c);
  }

  printf("\n");
  return 0;
}
