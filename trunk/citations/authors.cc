
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

using namespace rapidjson;

string NormalizeAuthor(string s) {
  string ret;
  ret.reserve(s.size());
  // Non-breaking space cause some strangeness
  s = Util::Replace(std::move(s), " ", " ");
  // Only count a word once per title.
  while (!s.empty()) {
    string word = Util::chop(s);
    if (!word.empty()) {
      word = Normalize(std::move(word));
      if (ret.empty()) ret = std::move(word);
      else ret += (string)" " + word;
    }
  }
  return ret;
}

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
    outfile = filenames[0] + ".authors";
  }
  
  if (filenames.empty()) {
    fprintf(stderr, "Pass JSON paper files on the command line.\n");
    return -1;
  }
  
  int64 counter = 0LL, no_authors = 0LL;

  // Number of articles authored by each name.
  std::unordered_map<string, CiteStats> author_stats;
  
  for (const string &filename : filenames) {
    LocalForEachLine(filename,
		     [&counter, &no_authors, &author_stats](string j) {
      Document article;
      CHECK(!article.Parse(j.c_str()).HasParseError());
      CHECK(article.IsObject());

      // Have to have authors or there's no way to count it.
      if (article.HasMember("authors")) {
	int64 n_citation = 0LL;
	if (article.HasMember("n_citation") &&
	    article["n_citation"].IsInt()) {
	  n_citation = article["n_citation"].GetInt();
	}
	  
	auto Count = [n_citation, &author_stats](const string &author_name) {
	  CiteStats &stats = author_stats[author_name];
	  stats.articles++;
	  stats.citations += n_citation;
	};


	const Value &authors = article["authors"];
	CHECK(authors.IsArray());
	for (const Value &author : authors.GetArray()) {
	  CHECK(author.IsObject());
	  
	  if (author.HasMember("name") && author["name"].IsString()) {
	    const string author_name =
	      Util::NormalizeWhitespace(author["name"].GetString());
	    Count(NormalizeAuthor(author_name));
	  } else {
	    Count("");
	  }
	}

      } else {
	no_authors++;
      }

      counter++;
    });

    printf("%lld articles. %lld with no author, %lld blank author\n",
	   counter, no_authors, author_stats[""].articles);
  }

  printf("Writing %lld author records to %s...\n", author_stats.size(),
	 outfile.c_str());
  FILE *out = fopen(outfile.c_str(), "wb");
  CHECK(out != nullptr) << outfile.c_str();
  for (const auto &row : author_stats) {
    fprintf(out, "%s\t%lld\t%lld\n",
	    row.first.c_str(), row.second.articles, row.second.citations);
  }
  fclose(out);

  return 0;
}
