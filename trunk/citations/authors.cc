
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "rapidjson/document.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "threadutil.h"
#include "citation-util.h"

#include <vector>
#include <string>
#include <map>
#include <unordered_map>

using namespace rapidjson;
using namespace std;

std::vector<string> AminerFiles() {
  return {
    "aminer_papers_0/aminer_papers_0.txt",
    "aminer_papers_0/aminer_papers_1.txt",
    "aminer_papers_0/aminer_papers_10.txt",
    "aminer_papers_0/aminer_papers_11.txt",
    "aminer_papers_0/aminer_papers_12.txt",
    "aminer_papers_0/aminer_papers_13.txt",
    "aminer_papers_0/aminer_papers_14.txt",
    "aminer_papers_0/aminer_papers_15.txt",
    "aminer_papers_0/aminer_papers_16.txt",
    "aminer_papers_0/aminer_papers_17.txt",
    "aminer_papers_0/aminer_papers_18.txt",
    "aminer_papers_0/aminer_papers_19.txt",
    "aminer_papers_0/aminer_papers_2.txt",
    "aminer_papers_0/aminer_papers_20.txt",
    "aminer_papers_0/aminer_papers_21.txt",
    "aminer_papers_0/aminer_papers_22.txt",
    "aminer_papers_0/aminer_papers_23.txt",
    "aminer_papers_0/aminer_papers_24.txt",
    "aminer_papers_0/aminer_papers_25.txt",
    "aminer_papers_0/aminer_papers_26.txt",
    "aminer_papers_0/aminer_papers_27.txt",
    "aminer_papers_0/aminer_papers_28.txt",
    "aminer_papers_0/aminer_papers_29.txt",
    "aminer_papers_0/aminer_papers_3.txt",
    "aminer_papers_0/aminer_papers_4.txt",
    "aminer_papers_0/aminer_papers_5.txt",
    "aminer_papers_0/aminer_papers_6.txt",
    "aminer_papers_0/aminer_papers_7.txt",
    "aminer_papers_0/aminer_papers_8.txt",
    "aminer_papers_0/aminer_papers_9.txt",

    "aminer_papers_1/aminer_papers_30.txt",
    "aminer_papers_1/aminer_papers_31.txt",
    "aminer_papers_1/aminer_papers_32.txt",
    "aminer_papers_1/aminer_papers_33.txt",
    "aminer_papers_1/aminer_papers_34.txt",
    "aminer_papers_1/aminer_papers_35.txt",
    "aminer_papers_1/aminer_papers_36.txt",
    "aminer_papers_1/aminer_papers_37.txt",
    "aminer_papers_1/aminer_papers_38.txt",
    "aminer_papers_1/aminer_papers_39.txt",
    "aminer_papers_1/aminer_papers_40.txt",
    "aminer_papers_1/aminer_papers_41.txt",
    "aminer_papers_1/aminer_papers_42.txt",
    "aminer_papers_1/aminer_papers_43.txt",
    "aminer_papers_1/aminer_papers_44.txt",
    "aminer_papers_1/aminer_papers_45.txt",
    "aminer_papers_1/aminer_papers_46.txt",
    "aminer_papers_1/aminer_papers_47.txt",
    "aminer_papers_1/aminer_papers_48.txt",
    "aminer_papers_1/aminer_papers_49.txt",
    "aminer_papers_1/aminer_papers_50.txt",
    "aminer_papers_1/aminer_papers_51.txt",
    "aminer_papers_1/aminer_papers_52.txt",
    "aminer_papers_1/aminer_papers_53.txt",
    "aminer_papers_1/aminer_papers_54.txt",
    "aminer_papers_1/aminer_papers_55.txt",
    "aminer_papers_1/aminer_papers_56.txt",
    "aminer_papers_1/aminer_papers_57.txt",
    "aminer_papers_1/aminer_papers_58.txt",

    "aminer_papers_2/aminer_papers_100.txt",
    "aminer_papers_2/aminer_papers_101.txt",
    "aminer_papers_2/aminer_papers_102.txt",
    "aminer_papers_2/aminer_papers_103.txt",
    "aminer_papers_2/aminer_papers_104.txt",
    "aminer_papers_2/aminer_papers_105.txt",
    "aminer_papers_2/aminer_papers_106.txt",
    "aminer_papers_2/aminer_papers_107.txt",
    "aminer_papers_2/aminer_papers_108.txt",
    "aminer_papers_2/aminer_papers_109.txt",
    "aminer_papers_2/aminer_papers_110.txt",
    "aminer_papers_2/aminer_papers_111.txt",
    "aminer_papers_2/aminer_papers_112.txt",
    "aminer_papers_2/aminer_papers_113.txt",
    "aminer_papers_2/aminer_papers_114.txt",
    "aminer_papers_2/aminer_papers_115.txt",
    "aminer_papers_2/aminer_papers_116.txt",
    "aminer_papers_2/aminer_papers_117.txt",
    "aminer_papers_2/aminer_papers_118.txt",
    "aminer_papers_2/aminer_papers_119.txt",
    "aminer_papers_2/aminer_papers_120.txt",
    "aminer_papers_2/aminer_papers_121.txt",
    "aminer_papers_2/aminer_papers_122.txt",
    "aminer_papers_2/aminer_papers_123.txt",
    "aminer_papers_2/aminer_papers_124.txt",
    "aminer_papers_2/aminer_papers_125.txt",
    "aminer_papers_2/aminer_papers_126.txt",
    "aminer_papers_2/aminer_papers_127.txt",
    "aminer_papers_2/aminer_papers_128.txt",
    "aminer_papers_2/aminer_papers_129.txt",
    "aminer_papers_2/aminer_papers_130.txt",
    "aminer_papers_2/aminer_papers_131.txt",
    "aminer_papers_2/aminer_papers_132.txt",
    "aminer_papers_2/aminer_papers_133.txt",
    "aminer_papers_2/aminer_papers_134.txt",
    "aminer_papers_2/aminer_papers_135.txt",
    "aminer_papers_2/aminer_papers_136.txt",
    "aminer_papers_2/aminer_papers_137.txt",
    "aminer_papers_2/aminer_papers_138.txt",
    "aminer_papers_2/aminer_papers_139.txt",
    "aminer_papers_2/aminer_papers_140.txt",
    "aminer_papers_2/aminer_papers_141.txt",
    "aminer_papers_2/aminer_papers_142.txt",
    "aminer_papers_2/aminer_papers_143.txt",
    "aminer_papers_2/aminer_papers_144.txt",
    "aminer_papers_2/aminer_papers_145.txt",
    "aminer_papers_2/aminer_papers_146.txt",
    "aminer_papers_2/aminer_papers_147.txt",
    "aminer_papers_2/aminer_papers_148.txt",
    "aminer_papers_2/aminer_papers_149.txt",
    "aminer_papers_2/aminer_papers_150.txt",
    "aminer_papers_2/aminer_papers_151.txt",
    "aminer_papers_2/aminer_papers_152.txt",
    "aminer_papers_2/aminer_papers_153.txt",
    "aminer_papers_2/aminer_papers_154.txt",
    "aminer_papers_2/aminer_papers_59.txt",
    "aminer_papers_2/aminer_papers_60.txt",
    "aminer_papers_2/aminer_papers_61.txt",
    "aminer_papers_2/aminer_papers_62.txt",
    "aminer_papers_2/aminer_papers_63.txt",
    "aminer_papers_2/aminer_papers_64.txt",
    "aminer_papers_2/aminer_papers_65.txt",
    "aminer_papers_2/aminer_papers_66.txt",
    "aminer_papers_2/aminer_papers_67.txt",
    "aminer_papers_2/aminer_papers_68.txt",
    "aminer_papers_2/aminer_papers_69.txt",
    "aminer_papers_2/aminer_papers_70.txt",
    "aminer_papers_2/aminer_papers_71.txt",
    "aminer_papers_2/aminer_papers_72.txt",
    "aminer_papers_2/aminer_papers_73.txt",
    "aminer_papers_2/aminer_papers_74.txt",
    "aminer_papers_2/aminer_papers_75.txt",
    "aminer_papers_2/aminer_papers_76.txt",
    "aminer_papers_2/aminer_papers_77.txt",
    "aminer_papers_2/aminer_papers_78.txt",
    "aminer_papers_2/aminer_papers_79.txt",
    "aminer_papers_2/aminer_papers_80.txt",
    "aminer_papers_2/aminer_papers_81.txt",
    "aminer_papers_2/aminer_papers_82.txt",
    "aminer_papers_2/aminer_papers_83.txt",
    "aminer_papers_2/aminer_papers_84.txt",
    "aminer_papers_2/aminer_papers_85.txt",
    "aminer_papers_2/aminer_papers_86.txt",
    "aminer_papers_2/aminer_papers_87.txt",
    "aminer_papers_2/aminer_papers_88.txt",
    "aminer_papers_2/aminer_papers_89.txt",
    "aminer_papers_2/aminer_papers_90.txt",
    "aminer_papers_2/aminer_papers_91.txt",
    "aminer_papers_2/aminer_papers_92.txt",
    "aminer_papers_2/aminer_papers_93.txt",
    "aminer_papers_2/aminer_papers_94.txt",
    "aminer_papers_2/aminer_papers_95.txt",
    "aminer_papers_2/aminer_papers_96.txt",
    "aminer_papers_2/aminer_papers_97.txt",
    "aminer_papers_2/aminer_papers_98.txt",
    "aminer_papers_2/aminer_papers_99.txt",
  };
}

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
  
  std::mutex m;
  int64 counter = 0LL, no_authors = 0LL;

  // Number of articles authored by each name.
  std::unordered_map<string, AuthorStats> author_stats;
  
  auto OneFile = 
    [&m, &counter, &no_authors, &author_stats](const string &filename) {
    LocalForEachLine(filename,
		     [&m, &counter, &no_authors, &author_stats](string j) {
      Document article;
      CHECK(!article.Parse(j.c_str()).HasParseError());
      CHECK(article.IsObject());

      MutexLock ml(&m);
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
	  
	  if (author.HasMember("name") && author["name"].IsString()) {
	    const string author_name =
	      Util::NormalizeWhitespace(author["name"].GetString());
	    Count(author_name);
	  } else {
	    Count("");
	  }
	}

      } else {
	no_authors++;
      }

      counter++;
    });

    {
      MutexLock ml(&m);
      printf("%lld articles. %lld with no author, %lld blank author\n",
	     counter, no_authors, author_stats[""].articles);
    }
  };    

  UnParallelApp(filenames, OneFile, 8);
  
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
