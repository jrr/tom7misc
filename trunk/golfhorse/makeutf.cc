#include <cstdint>
#include <stdio.h>

using uint8 = uint8_t;

int main (int argc, char **argv) {
  printf("let r=[");
  for (int i = 128; i < 2048; i++) {
    uint8 b1 = 0b110'00000;
    uint8 b2 = 0b10'000000;

    b1 |= (i >> 6) & 0b11111;
    b2 |= i & 0b111111;
    printf("'e%c%co',", b1, b2);
  }
  printf("];\n");

  printf(R"(
for (let i = 0; i < r.length; i++) {
  if (r[i].length != 3 || r[i][1] != r[i].substr(1, 1)) {
    console.log(i, ' no: ', r[i], " len ", r[i].length, 
                " ", r[i].charCodeAt(0), " ", r[i].charCodeAt(1));
  }
  if (r[i].split(r[i][1]).join('z') != 'ezo')
    console.log(i, 'split/join');
}
)");

  
  
  return 0;
}
