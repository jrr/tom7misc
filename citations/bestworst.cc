
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

#include <cmath>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>

struct Stats {
  int64 articles = 0;
  int64 citations = 0;
  // citations per article, [XXX but assuming one free article and one free citation?]
  double citation_rate = 0.0;
};

int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  CHECK_EQ(argc, 4) << "bestworst.exe infile.txt output-best.txt ouput-worst.txt\n";
  string infile = argv[1];
  string bestfile = argv[2];
  string worstfile = argv[3];

  std::unordered_map<string, Stats> cite_stats;
  int64 articles_kept = 0LL, citations_kept = 0LL;
  RE2 line_re{"([^\t]*)\t([\\d]+)\t([\\d]+)"};  
  LocalForEachLine(infile,
		   [&cite_stats, &line_re,
		    &articles_kept, &citations_kept](string line) {
    string author;
    int64 articles = 0LL, citations = 0LL;
    CHECK(RE2::FullMatch(line, line_re, &author, &articles, &citations)) << line;

    Stats &stats = cite_stats[author];
    stats.articles += articles;
    stats.citations += citations;
    articles_kept += articles;
    citations_kept += citations;
  });

  for (auto &p : cite_stats) {
    p.second.citation_rate = (double)(p.second.citations + 1LL) /
      (double)(p.second.articles + 1LL);
  }
  
  printf("Got stats for %lld keys.\n"
	 "Total kept articles: %lld  and citations: %lld\n"
	 "Average citations per paper: %.04f\n",
	 (int64)cite_stats.size(), 
	 articles_kept, citations_kept,
	 (double)citations_kept / articles_kept);
  
  vector<std::pair<string, Stats>> rows;
  rows.reserve(cite_stats.size());
  for (const auto &p : cite_stats) {
    rows.emplace_back(p.first, p.second);
  }
  cite_stats.clear();

  // Sort descending by citation rate.
  std::sort(rows.begin(), rows.end(),
	    [](const std::pair<string, Stats> &a,
	       const std::pair<string, Stats> &b) {
	      return a.second.citation_rate > b.second.citation_rate;
	    });

  printf("Sorted.\n");

  // Now write output files.
  {
    FILE *out = fopen(bestfile.c_str(), "wb");
    CHECK(out != nullptr) << bestfile.c_str();
    for (int i = 0; i < 5000 && i < rows.size(); i++) {
      const auto &row = rows[i];
      fprintf(out, "%d.  %s\t%lld\t%lld\t%.6f\n",
	      i, row.first.c_str(), row.second.articles, row.second.citations,
	      row.second.citation_rate);
    }
    printf("Wrote %s.\n", bestfile.c_str());
    fclose(out);
  }

  {
    FILE *out = fopen(worstfile.c_str(), "wb");
    CHECK(out != nullptr) << worstfile.c_str();
    for (int i = rows.size() - 1; i >= 0 && i >= rows.size() - 5000; i--) {
      const auto &row = rows[i];
      fprintf(out, "%d.  %s\t%lld\t%lld\t%.6f\n",
	      i, row.first.c_str(), row.second.articles, row.second.citations,
	      row.second.citation_rate);
    }
    printf("Wrote %s.\n", worstfile.c_str());
    fclose(out);
  }

  return 0;
}
