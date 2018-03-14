
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "threadutil.h"
#include "re2/re2.h"
#include "citation-util.h"
#include "threadutil.h"

#include <cmath>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>

struct Stats {
  int64 articles = 0;
  int64 citations = 0;
  // this is (articles / citations) / average_citation_rate.
  double citation_multiplier = 0.0;
};

struct Title {
  string id;
  string title;
  double cite_probability = 0.0;
  Title(string id, string title) :
    id(std::move(id)), title(std::move(title)) {}
};

int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  CHECK_GE(argc, 5) << "worstpapers.exe words.txt best-output-papers.txt worst-output-papers.txt title1.txt title2.txt ... titlen.txt\n";
  string wordfile = argv[1];
  string bestfile = argv[2];
  string worstfile = argv[3];
  
  vector<string> titlefiles;
  for (int i = 4; i < argc; i++) {
    titlefiles.emplace_back(argv[i]);
  }

  vector<Title> titles;
  RE2 title_line_re{"([^\t]*)\t([^\t]*)"};
  for (const string &f : titlefiles) {
    printf("%s...\n", f.c_str());
    vector<string> lines = Util::ReadFileToLines(f);
    titles.reserve(titles.size() + lines.size());
    for (const string &line : lines) {
      string id, title;
      CHECK(RE2::FullMatch(line, title_line_re, &id, &title)) << line;
      titles.emplace_back(std::move(id), std::move(title));
    }
  }

  printf("Read %lld titles.\n", titles.size());

  static constexpr int64 MIN_ARTICLES = 100;
  static constexpr int64 MIN_CITATIONS = 1;
  
  // Now, probabilities for all words.
  printf("Reading words...");
  std::unordered_map<string, Stats> word_stats;
  int64 articles_kept = 0LL, citations_kept = 0LL;
  RE2 line_re{"([^\t]*)\t([\\d]+)\t([\\d]+)"};  
  for (const string &line : Util::ReadFileToLines(wordfile)) {
    string word;
    int64 articles = 0LL, citations = 0LL;
    CHECK(RE2::FullMatch(line, line_re, &word, &articles, &citations)) << line;

    if (citations >= MIN_CITATIONS &&
	articles >= MIN_ARTICLES &&
	IsAllAscii(word)) {
      Stats &stats = word_stats[word];
      stats.articles += articles;
      stats.citations += citations;
      articles_kept += articles;
      citations_kept += citations;
    }
  };
  printf(" got %lld.\n", word_stats.size());
  
  // Average citation rate (over all title words).
  const double avg = (double)citations_kept / articles_kept;
  const double inv_avg = 1.0 / avg;
  printf("Set citation rates...\n");
  for (auto &p : word_stats) {
    p.second.citation_multiplier =
      ((double)(p.second.citations) /
       (double)(p.second.articles)) * inv_avg;
  }

  printf("Got stats for %lld words.\n"
	 "Total kept articles: %lld  and citations: %lld\n"
	 "Average citations per paper: %.04f\n",
	 (int64)word_stats.size(), 
	 articles_kept, citations_kept,
	 avg);

  // Now score each title.
  ParallelComp(
      titles.size(),
      [&titles, &word_stats](int title_idx) {
	string title = titles[title_idx].title;
	double prob = 1.0;
	while (!title.empty()) {
	  string token = Normalize(Util::chop(title));
	  auto it = word_stats.find(token);
	  if (it == word_stats.end())
	    continue;
	  prob *= it->second.citation_multiplier;
	}
	titles[title_idx].cite_probability = prob;
      },
      12);
  printf("Scored titles.\n");

  // Sort descending by citation rate.
  std::sort(titles.begin(), titles.end(),
	    [](const Title &a,
	       const Title &b) {
	      return a.cite_probability > b.cite_probability;
	    });

  printf("Sorted.\n");

  // Now write output files.
  {
    FILE *out = fopen(bestfile.c_str(), "wb");
    CHECK(out != nullptr) << bestfile.c_str();
    for (int i = 0; i < 5000 && i < titles.size(); i++) {
      const Title &title = titles[i];
      fprintf(out, "%d.  %.9g\t%s\t%s\n",
	      i,
	      title.cite_probability,
	      title.id.c_str(), title.title.c_str());
    }
    printf("Wrote %s.\n", bestfile.c_str());
    fclose(out);
  }
  
  {
    FILE *out = fopen(worstfile.c_str(), "wb");
    CHECK(out != nullptr) << worstfile.c_str();
    for (int i = titles.size() - 1; i >= 0 && i >= titles.size() - 5000; i--) {
      const Title &title = titles[i];
      fprintf(out, "%d.  %.9g\t%s\t%s\n",
	      i,
	      title.cite_probability,
	      title.id.c_str(), title.title.c_str());
    }
    printf("Wrote %s.\n", worstfile.c_str());
    fclose(out);
  }
  
  return 0;
}
