#include "base/logging.h"
#include "base/stringprintf.h"

#include <sys/stat.h>
#include <stdio.h>

#include <vector>
#include <string>

using namespace std;

int main(int argc, char **argv) {
  // An early write to cerr will fix it...
  // std::cerr << "hi " << " hi\n";

#if 1
  FILE *f = fopen("aminer_papers_1/aminer_papers_44.txt", "rb");
  int fd = fileno(f);
  struct stat st;
  if (0 != fstat(fd, &st)) {
    printf("NO\n");
  }

  string ret;
  off_t next_pos = 0;
  off_t size_guess = st.st_size + 1;
  //    if (magic_opt != nullptr) {
  //      ret = *magic_opt;
  //      next_pos = ret.size();
  //    }

  for (;;) {
    // Now, repeatedly, resize the string to accommodate a
    // read that we think will finish off the file. But
    // don't do zero-sized writes!
    if (next_pos >= size_guess) {
      // XXX possibility for overflow, ugh
      size_guess = next_pos + 16;
    }
    printf("Resize buffer to %d\n", (int)size_guess);
    ret.resize(size_guess);
    const off_t read_size = size_guess - next_pos;
    printf("Attempt to read %d bytes\n", (int)read_size);

    // Bytes are required to be contiguous from C++11;
    // use .front() instead of [next_pos] since the former,
    // introduced in C++11, will prove we have a compatible
    // version.
    const size_t bytes_read =
      fread(&ret.front() + next_pos, 1, read_size, f);
    printf("%d bytes were read\n", (int)bytes_read);

    // We read exactly this many bytes.
    next_pos += bytes_read;
    printf("Now next_pos is %d\n", (int)next_pos);
    if (feof(f)) {
      printf("EOF. ret size is %d. resize to %d.\n", (int)ret.size(), (int)next_pos);
      // Should be no-op when we guessed correctly.
      ret.resize(next_pos);
      fclose(f);
      break;
    } else {
      printf("Not EOF.\n");
    }

    // If we're not at the end of file but no bytes were
    // read, then something is amiss. This also ensures
    // that the loop makes progress.
    if (bytes_read == 0) {
      fclose(f);
      printf("NO2\n");
      return 0;
    }
  }

  // Using cerr here too also avoids the crash.
  // std::cerr << "now " << " split to lines";

  vector<string> v;
  string line;
  // PERF don't need to do so much copying.
  printf("sizeof size_t %d\n", sizeof (size_t));
  for (size_t i = 0; i < ret.size(); i++) {
    if (ret[i] == '\r') {
      continue;
    } else if (ret[i] == '\n') {
      v.push_back(line);
      line = "";
    } else {
      line += ret[i];
    }
  }
  printf("Got past lines\n");
#endif

  // cerr crashes after the first "bye ", so weird...
  std::cerr << "bye " << " bye\n";

  return 0;
}
