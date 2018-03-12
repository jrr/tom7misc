
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "threadutil.h"
#include "re2/re2.h"
#include "textsvg.h"
#include "citation-util.h"
#include "randutil.h"
#include "arcfour.h"

#include <cmath>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>

int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  CHECK_EQ(argc, 2) << "tourney.exe infile.txt\n";
  string infile = argv[1];

  // Number of articles authored by each name.
  std::unordered_map<string, CiteStats> cite_stats;
  int64 authors_bad = 0LL, articles_bad = 0LL;
  int64 articles_kept = 0LL, citations_kept = 0LL;
  RE2 line_re{"([^\t]*)\t([\\d]+)\t([\\d]+)"};  
  LocalForEachLine(infile,
		   [&cite_stats, &line_re, &authors_bad, &articles_bad,
		    &articles_kept, &citations_kept](string line) {
    string author;
    int64 articles = 0LL, citations = 0LL;
    CHECK(RE2::FullMatch(line, line_re, &author, &articles, &citations)) <<
      line;

    string dict;
    if (Dictionaryize<false>(author, &dict)) {
      CiteStats &stats = cite_stats[dict];
      stats.articles += articles;
      stats.citations += citations;
      articles_kept += articles;
      citations_kept += citations;
    } else {
      authors_bad++;
      articles_bad += articles;
    }
  });

  printf("Got stats for %lld keys, "
	 "with %lld rejected (%lld articles rejected)\n"
	 "Total kept articles: %lld  and citations: %lld\n",
	 (int64)cite_stats.size(), authors_bad, articles_bad,
	 articles_kept, citations_kept);
  
  vector<std::pair<string, CiteStats>> rows;
  rows.reserve(cite_stats.size());
  for (const auto &p : cite_stats) {
    rows.emplace_back(p.first, p.second);
  }

  std::sort(rows.begin(), rows.end(),
	    [](const std::pair<string, CiteStats> &a,
	       const std::pair<string, CiteStats> &b) {
	      return a.first < b.first;
	    });

  printf("Sorted.\n");

  // Perform tournament.
  ArcFour rc("tourney0");
  
  // (left means alphabetically earlier; right later.)
  // Total number of citations for each side.
  int64 left_citations = 0LL, right_citations = 0LL;
  int64 left_articles = 0LL, right_articles = 0LL;
  int64 left_won = 0LL, right_won = 0LL;
  for (int64 rounds = 0LL; true; rounds++) {
    int64 idx1 = RandTo(&rc, rows.size());
    int64 idx2;
    do {
      idx2 = RandTo(&rc, rows.size());
    } while (idx1 == idx2);

    const int64 left = std::min(idx1, idx2);
    const int64 right = std::max(idx1, idx2);

    const int64 lc = rows[left].second.citations;
    const int64 la = rows[left].second.articles;
    left_citations += lc;
    left_articles += la;

    const int64 rc = rows[right].second.citations;
    const int64 ra = rows[right].second.articles;
    right_citations += rc;
    right_articles += ra;

    const double lrate = (double)lc / la;
    const double rrate = (double)rc / ra;
    
    if (lrate > rrate) left_won++;
    else if (rrate > lrate) right_won++;
    
    if (rounds % 12345678 == 0) {
      printf(
	  "%.3f Mrounds:\n"
	  "  (%.2f vs. %.2f Gcit) (%.2f vs. %.2f Gart) (%.3f vs. %.3f Gwin)\n"
	  "  (%.6f L:R cit) (%.6f L:R art) (%.6f vs. %.6f win rate)\n",
	  (double)rounds / 1e6,
	  left_citations / 1e9, right_citations / 1e9,
	  left_articles / 1e9, right_articles / 1e9,
	  left_won / 1e9, right_won / 1e9,
	  ((double)left_citations / right_citations),
	  ((double)left_articles / right_articles),
	  ((double)left_won / rounds), ((double)right_won / rounds));
    }
  }
  
  return 0;
}
