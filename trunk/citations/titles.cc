
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "rapidjson/document.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "citation-util.h"

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
  
  vector<string> filenames;
  string outfile;
  for (int i = 1; i < argc; i++) {
    string arg = (string)argv[i];
    if (arg == "-o") {
      CHECK(i < argc - 1);
      outfile = argv[i + 1];
      i++;
    } else {
      filenames.push_back(arg);
    }
  }

  if (outfile.empty()) {
    outfile = filenames[0] + ".titles";
  }
  
  if (filenames.empty()) {
    fprintf(stderr, "Pass JSON paper files on the command line.\n");
    return -1;
  }
  
  int64 counter = 0LL, num_output = 0LL, not_ascii = 0LL, not_en = 0LL;
  int64 has_citations = 0LL;
  
  FILE *out = fopen(outfile.c_str(), "wb");

  for (const string &filename : filenames) {
    LocalForEachLine(filename,
		     [&counter, &has_citations,
		      &num_output, &not_ascii, &not_en,
		      out](string j) {
      counter++;
      Document article;
      CHECK(!article.Parse(j.c_str()).HasParseError());
      CHECK(article.IsObject());

      // Have to have a title or there's no way to count it.
      CHECK(article.HasMember("id")) << j;
      const Value &id_value = article["id"];
      CHECK(id_value.IsString()) << j;
      const string id = id_value.GetString();
      
      if (article.HasMember("title")) {
	int64 n_citation = 0LL;
	if (article.HasMember("n_citation") &&
	    article["n_citation"].IsInt()) {
	  n_citation = article["n_citation"].GetInt();
	}

	// Papers with citations can't be the least-citable.
	if (n_citation > 0) {
	  has_citations++;
	  return;
	}

	// Require them to be in English.
	if (!article.HasMember("lang")) {
	  not_en++;
	  return;
	}
	const Value &lang = article["lang"];
	if (!lang.IsString() ||
	    (string)lang.GetString() != "en") {
	  not_en++;
	  return;
	}
	
	const Value &title = article["title"];
	if (title.IsString()) {
	  string words = LightNormalization(title.GetString());

	  if (IsAllAscii(words)) {
	    fprintf(out, "%s\t%s\n", id.c_str(), words.c_str());
	    num_output++;
	  } else {
	    not_ascii++;
	  }
	}
      }
    });

    printf("%lld articles. %lld has citations.\n"
	   "%lld not ascii. %lld not en. %lld kept\n",
	   counter, has_citations, not_ascii, not_en, num_output);
  }
  
  printf("Wrote %s\n", outfile.c_str());
  fclose(out);

  return 0;
}
