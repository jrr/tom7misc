
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

  std::unordered_set<string> interesting = {
    "53e9a66eb7602d9702fa2ee0", // grid grid grid
    // crap
    "56d8981adabfae2eee20314b",
    "56d8a690dabfae2eee9057b0",
  };

  vector<string> filenames;
  for (int i = 1; i < argc; i++) {
    filenames.emplace_back(argv[i]);
  }

  string outfile = "interesting.txt";
  
  if (filenames.empty()) {
    fprintf(stderr, "Pass JSON paper files on the command line.\n");
    return -1;
  }

  std::mutex mutex;
  FILE *out = fopen(outfile.c_str(), "wb");

  int64 done = 0LL;
  UnParallelApp(
      filenames,
      [&filenames, &interesting, &mutex, out, &done](
	  const string &filename) {
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

	{
	  MutexLock ml(&mutex);
	  done++;
	  printf("Processed %lld/%lld.\n", done, filenames.size());
	}
      },
      10);
  
  printf("Wrote %s\n", outfile.c_str());
  fclose(out);

  return 0;
}
