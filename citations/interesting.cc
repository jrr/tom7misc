
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "rapidjson/document.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "citation-util.h"
#include "threadutil.h"

#include <mutex>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>
#include <unordered_set>

using namespace rapidjson;
using namespace std;

int main(int argc, char **argv) {
#ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
#endif

  CHECK_EQ(argc, 2) << "./interesting.exe filename";
  const string filename = argv[1];
  
  std::unordered_set<string> interesting = {
    "53e9a66eb7602d9702fa2ee0", // grid grid grid
    // loads of repeated articles like this.
    // were worst before filtering lang=en
    "56d8981adabfae2eee20314b",
    "56d8a690dabfae2eee9057b0",
    // some of the first few english articles I found
    // in worstpapers, but they seem to be broken records
    "56d81919dabfae2eee86f82a",
    "53e9a089b7602d970296e0eb",
    // maybe actually real one, poets
    "56d918ecdabfae2eee6c6965",
    "53e99952b7602d9702190ac1",
    "56d8ecc5dabfae2eee5b30f5",
    "56d9266ddabfae2eeebdd19a",
    "56d85cd0dabfae2eee5cb5f4",
    "53e9bce1b7602d9704965e64",
  };

  string outfile = filename + ".interesting";
  
  if (filenames.empty()) {
    fprintf(stderr, "Pass JSON paper files on the command line.\n");
    return -1;
  }

  FILE *out = fopen(outfile.c_str(), "wb");

  int64 done = 0LL;

  for (const string &j : Util::ReadFileToLines(filename)) {
    Document article;
    CHECK(!article.Parse(j.c_str()).HasParseError());
    CHECK(article.IsObject());
	    
    CHECK(article.HasMember("id")) << j;
    const Value &id_value = article["id"];
    CHECK(id_value.IsString()) << j;
    const string id = id_value.GetString();

    if (ContainsKey(interesting, id)) {
      MutexLock ml(&mutex);
      printf("Found %s: %s\n", id.c_str(), j.c_str());
      fprintf(out, "%s\n", j.c_str());
    }
  }

  printf("Wrote %s\n", outfile.c_str());
  fclose(out);

  return 0;
}
