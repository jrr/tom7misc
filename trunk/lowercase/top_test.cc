
#include <vector>

#include "top.h"

using namespace std;

int main(int argc, char **argv) {

  for (const string &proc : Top::Enumerate()) {
    printf("%s\n", proc.c_str());
  }
  
  return 0;
}
