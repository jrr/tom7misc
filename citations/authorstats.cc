
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

#include <cmath>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>

template<class T>
void Reverse(vector<T> *v) {
  vector<T> ret;
  ret.reserve(v->size());
  for (int i = v->size() - 1; i >= 0; i--)
    ret.push_back(std::move((*v)[i]));
  v->swap(ret);
}

bool Dictionaryize(string author, string *dict) {
  vector<string> tokens;
  while (!author.empty()) {
    string tok = Util::chop(author);
    if (!tok.empty()) tokens.push_back(Util::lcase(tok));
  }

  // Totally whitespace name?
  if (tokens.empty())
    return false;

  Reverse(&tokens);
  
  // Make sure it fits somewhere in alphabetical order.
  if (tokens[0][0] >= 'a' && tokens[0][0] <= 'z') {
    dict->clear();
    for (const string &tok : tokens) {
      if (dict->empty()) {
	*dict = tok;
      } else {
	*dict += " ";
	*dict += tok;
      }
    }
    return true;
  } else {
    return false;
  }
}

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  CHECK_EQ(argc, 3) << "authorstats.exe infile.txt outfile.svg\n";
  string infile = argv[1];
  string outfile = argv[2];

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
    CHECK(RE2::FullMatch(line, line_re, &author, &articles, &citations)) << line;

    string dict;
    if (Dictionaryize(author, &dict)) {
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

  printf("Got stats for %lld keys, with %lld rejected (%lld articles rejected)\n"
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

  // Now write CDFs.
  {
    constexpr double XSCALE = 1024.0, YSCALE = 1024.0;
    string articles_cdf =
      "<polyline stroke-linejoin=\"round\" "
      "fill=\"none\" stroke=\"#800\" stroke-opacity=\"0.75\" "
      "stroke-width=\"1.5\" points=\"";
    articles_cdf.reserve(1 << 23);
    string citations_cdf =
      "<polyline stroke-linejoin=\"round\" "
      "fill=\"none\" stroke=\"#008\" stroke-opacity=\"0.75\" "
      "stroke-width=\"1.5\" points=\"";
    citations_cdf.reserve(1 << 23);
    
    int64 articles = 0LL, citations = 0LL;
    for (int i = 0; i < rows.size(); i++) {
      articles += rows[i].second.articles;
      citations += rows[i].second.citations;
      if (i % 10000 == 0) {
	// Rank of author
	double x = i / (double)rows.size();
	double ay = 1.0 - (articles / (double)articles_kept);
	double cy = 1.0 - (citations / (double)citations_kept);
	articles_cdf +=
	  StringPrintf("%s,%s ", Rtos(x * XSCALE).c_str(), Rtos(ay * YSCALE).c_str());
	citations_cdf +=
	  StringPrintf("%s,%s ", Rtos(x * XSCALE).c_str(), Rtos(cy * YSCALE).c_str());
      }
    }

    string svg = TextSVG::Header(XSCALE, YSCALE);
    svg += articles_cdf + "\" />\n";
    svg += citations_cdf + "\" />\n";
    svg += TextSVG::Footer();
    Util::WriteFile(outfile, svg);
    printf("Wrote %s\n", outfile.c_str());
  }
  
  string sorted_outfile = infile + "sorted.txt";
  printf("Writing %lld sorted records to %s...\n", rows.size(),
	 sorted_outfile.c_str());
  FILE *out = fopen(sorted_outfile.c_str(), "wb");
  CHECK(out != nullptr) << outfile.c_str();
  for (const auto &row : rows) {
    fprintf(out, "%s\t%lld\t%lld\n",
	    row.first.c_str(), row.second.articles, row.second.citations);
  }
  fclose(out);
  
  return 0;
}
