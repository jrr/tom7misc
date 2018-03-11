
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
    outfile = filenames[0] + ".words";
  }
  
  if (filenames.empty()) {
    fprintf(stderr, "Pass JSON paper files on the command line.\n");
    return -1;
  }
  
  int64 counter = 0LL, no_title = 0LL;

  // Stats for each title word.
  std::unordered_map<string, CiteStats> word_stats;

  for (const string &filename : filenames) {
    LocalForEachLine(filename,
		     [&counter, &no_title, &word_stats](string j) {
      Document article;
      CHECK(!article.Parse(j.c_str()).HasParseError());
      CHECK(article.IsObject());

      // Have to have a title or there's no way to count it.
      if (article.HasMember("title")) {
	int64 n_citation = 0LL;
	if (article.HasMember("n_citation") &&
	    article["n_citation"].IsInt()) {
	  n_citation = article["n_citation"].GetInt();
	}
	
	auto Count = [n_citation, &word_stats](const string &word) {
	  CiteStats &stats = word_stats[word];
	  stats.articles++;
	  stats.citations += n_citation;
	};

	const Value &title = article["title"];
	if (title.IsString()) {
	  string words = title.GetString();
	  // Non-breaking space?
	  words = Util::Replace(std::move(words), " ", " ");
	  // Only count a word once per title.
	  std::unordered_set<string> boolean_words;
	  while (!words.empty()) {
	    string word = Util::chop(words);
	    if (!word.empty()) {
	      boolean_words.insert(Normalize(std::move(word)));
	    }
	  }
	  for (const string &w : boolean_words) Count(w);
	}

      } else {
	no_title++;
      }

      counter++;
    });

    printf("%lld articles. %lld with no title. %lld words\n",
	   counter, no_title, word_stats.size());
  }
  
  printf("Writing %lld word records to %s...\n", word_stats.size(),
	 outfile.c_str());
  FILE *out = fopen(outfile.c_str(), "wb");
  CHECK(out != nullptr) << outfile.c_str();
  for (const auto &row : word_stats) {
    fprintf(out, "%s\t%lld\t%lld\n",
	    row.first.c_str(), row.second.articles, row.second.citations);
  }
  fclose(out);

  return 0;
}
