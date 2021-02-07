#include <stdio.h>
#include <string.h>
#include <cstdint>

#include "sha256.h"
#include "base/logging.h"

int main(int argc, char **argv) {

  CHECK(SHA256::Ascii(SHA256::HashString("")) ==
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");

  CHECK(SHA256::Ascii(SHA256::HashString("ponycorn")) ==
        "6be4ca413e5958953587238dda80aa399b03f1ad7a61ea681d53e1f69e6daa15");
  
  return 0;
}
