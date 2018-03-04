
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "rapidjson/document.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"

#include <vector>
#include <string>
#include <map>
#include <unordered_map>

using namespace rapidjson;
using namespace std;

struct AuthorStats {
  int64 articles = 0;
  int64 citations = 0;
};

int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  string filename = "aminer_papers_1/aminer_papers_44.txt";
  string outfile = "deleteme.txt";
  
  int64 counter = 0LL, no_authors = 0LL;

  // Number of articles authored by each name.
  std::unordered_map<string, AuthorStats> author_stats;

  vector<string> lines = Util::ReadFileToLines(filename);
  
  const string &j = lines[490275];
  printf("[%s]\n", j.c_str());
  
  Document article;
  CHECK(!article.Parse(j.c_str()).HasParseError());
  CHECK(article.IsObject());


  // MutexLock ml(&m);
  // Have to have authors or there's no way to count it.
  if (article.HasMember("authors")) {
    int64 n_citation = 0LL;
    if (article.HasMember("n_citation") &&
	article["n_citation"].IsInt()) {
      n_citation = article["n_citation"].GetInt();
    }

    auto Count = [n_citation, &author_stats](const string &author_name) {
      AuthorStats &stats = author_stats[author_name];
      stats.articles++;
      stats.citations += n_citation;
    };


    const Value &authors = article["authors"];
    CHECK(authors.IsArray());
    for (const Value &author : authors.GetArray()) {
      CHECK(author.IsObject());

      if (author.HasMember("name")) {
	if (!author["name"].IsString())
	  printf("EXIT\n");
	CHECK(author["name"].IsString());
	const char *author_name = author["name"].GetString();
	// counter += strlen(author_name);
	// printf("(%s)\n", author_name.c_str());
	// string norm_author_name = Util::NormalizeWhitespace(author_name);
	// printf("[%s]\n", norm_author_name.c_str());
	/*
	Count(author_name);
	*/
      } else {
	Count("");
      }
    }

  } else {
    no_authors++;
  }

  counter++;

  printf("%lld articles. %lld with no author, %lld blank author\n",
	 counter, no_authors, author_stats[""].articles);

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
