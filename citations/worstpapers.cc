
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
#include <unordered_set>

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
  titles.reserve(91000000);
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
  static constexpr int64 MAX_TITLE_WORDS = 20;
  
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

  // Bunch of Indonesian(?) papers flood the worst 5000. Try to
  // filter them out so that we get some English ones.
  std::unordered_set<string> blacklist = {
    "analisa",
    "analisis",
    "dampak",
    "desentralisasi",
    "dimensi",
    "ekonomi",
    "evaluasi",
    "faktor-faktor",
    "gambaran",
    "hubungan",
    "implementasi",
    "keefektifan",
    "kesesuaian",
    "kewenangan",
    "kontribusi",
    "kualitas",
    "meningkatkan",
    "pelaksanaan",
    "pembangunan",
    "penentuan",
    "pengaruh",
    "pengetahuan",
    "peranan",
    "perancangan",
    "perawat",
    "perbedaan",
    "persepsi",
    "praperadilan",
    "proporsi",
    "prosedur",
    "reformasi",
    "sistem",
    "strategi",
    "tahun",
    "tanah",
    "tangga",
    "tanggung",
    "teknik",
    "komparasi",
    "telekomunikasi",
    "tentang",
    "terhadap",
    "tiga",
    "timur",
    "tindak",
    "tinggal",
    "tinggi",
    "tingkat",
    "tinjauan",
    "tradisional",
    "transaksi",
    "transmigrasi",
    "tuberkulosis",
    "tulungagung",
    "tumbuhan",
    "uang",
    "uin",
    "uji",
    "undang",
    "undang-undang",
    "universitas",
    "upaya",
    "usaha",
    "usia",
    "utara",
    "warlaba",
    "yogyakarta",
    "di",
    "dan",
    "pada",
    "dalam",
    "studi",
    "dengan",
    "siswa",
    "kabupaten",
    "kelas",
    "kasus",
    "pembelajaran",
    "kecamatan",
    "untuk",
    "negeri",
    "peningkatan",
    "kota",
    "belajar",
    "pengembangan",
    "yang",
    "melalui",
    "sebagai",
    "sekolah",
    "rumah",
    "kerja",
    "metode",
    "desa",
    "daerah",
    "sakit",
    "anak",
    "masyarakat",
    "menggunakan",
    "hasil",
    "dari",
    "dasar",
    "kemampuan",
    "penerapan",
    "berbasis",
    "medan",
    "pendidikan",
    "pendekatan",
    "matematika",
    "jawa",
    "bagi",
    "materi",
    "malang",
    "umum",
    "barat",
    "bahan",
    "hukum",
    "selatan",
    "faktor",
    "pelajaran",
    "pelayanan",
    "laporan",
    "peran",
    "mata",
    "sma",
    "surabaya",
    "kinerja",
    "smk",
    "kesehatan",
    "ditinjau",
    "sosial",
    "semarang",
    "penggunaan",
    "kelompok",
    "keterampilan",
    "kajian",
    "bandung",
    "antara",
    "pemanfaatan",
    "tengah",
    "menulis",
    "sdn",
    "industri",
    "deskriptif",
    "provinsi",
    "guru",
    "perusahaan",
    "komunikasi",
    "masalah",
    "pusat",
    "pola",
    "keluarga",
    "pajak",
    "ruang",
    "produksi",
    "lingkungan",
    "efektivitas",

    "|",
    "de",
    "la",
    "w",
    "canvey",
    "en",
    "und",
    "et",
    "des",
    "le",
    "del",
    "e",
    "du",
    "na",
    "der",
    "el",
    "les",
    "y",
    "wplyw",
    "della",
    "dans",
    "oraz",
    "von",
    "z",
    "recenzja",
    "il",
    "legale",
    "annonce",
    "sarl",
    "museen",
    "spanien",
    "un",
    "zur",
    "eurl",
    "unipersonnelle",
    "sur",
    "im",
    "dei",
    "agence",
    "photographie",
    "warszawa",
    "los",
    "m",
    "au",
    "rol",
    "s",
    "zum",
    "delle",
    "nawozenia",
    "einer",
    "roslin",
    "dm",
    "ochrony",
    "kunst",
    "verlag",
    "od",
    "verfahren",
    "una",
    "museo",
    "kultur",
    "las",
    "vorrichtung",
    "zawartosc",
    "het",
    "mit",
    "auf",
    "nella",
    "nel",
    "wybranych",
    "zaleznosci",
    "droit",
    "al",
    "tra",
    "alla",
    "h",
    "ein",
    "o",
    "diritto",
    "zu",
    "seiten",
    "ocena",
    "jakosc",
    "degli",
    "pszenicy",
    "odmian",
    "czesc",
    "ich",
    "septembre",
    "octobre",
    "para",
    "een",
    "siecle",
    "aux",
    "polsce",
    "r",
    "une",
    "latach",
    "sklad",
    "noir",
    "preis",
    "wartosc",
    "j",
    "eines",
    "produkcji",
    "avec",
    "internazionale",

    // Try to exclude broken book citations
    "hardcover",
    "pp",
    "isbn",
    "issn",
    "hardback",
    "paperback",
    "p",
    "pages",
    "isbn-13",
    
  };
  
  // Now score each title.
  ParallelComp(
      titles.size(),
      [&titles, &word_stats, &blacklist](int title_idx) {
	string title = titles[title_idx].title;
	if (title.find("$") != string::npos) {
	  titles[title_idx].cite_probability = -1;
	  return;
	}
	double prob = 1.0;
	int words = 0;
	while (!title.empty()) {
	  string token = Normalize(Util::chop(title));
	  if (ContainsKey(blacklist, token)) {
	    prob = -1.0;
	    break;
	  }
	  words++;
	  auto it = word_stats.find(token);
	  if (it == word_stats.end())
	    continue;
	  prob *= it->second.citation_multiplier;
	}
	if (words <= MAX_TITLE_WORDS) {
	  titles[title_idx].cite_probability = prob;
	} else {
	  // This will cause it to be deleted below.
	  titles[title_idx].cite_probability = -1.0;
	}
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

  int64 too_long = 0;
  while (!titles.empty() && titles.back().cite_probability < 0.0) {
    titles.pop_back();
    too_long++;
  }

  printf("Dropped %lld titles that were too long.\n", too_long);
  
  // Now write output files.
  {
    FILE *out = fopen(bestfile.c_str(), "wb");
    CHECK(out != nullptr) << bestfile.c_str();
    for (int i = 0; i < 5000 && i < titles.size(); i++) {
      const Title &title = titles[i];
      fprintf(out, "%d.  %.17g\t%s\t%s\n",
	      i,
	      title.cite_probability,
	      title.id.c_str(), title.title.c_str());
    }
    printf("Wrote %s.\n", bestfile.c_str());
    fclose(out);
  }

  std::unordered_map<string, int> worstwordcounts;
  
  {
    FILE *out = fopen(worstfile.c_str(), "wb");
    CHECK(out != nullptr) << worstfile.c_str();
    for (int i = titles.size() - 1; i >= 0 && i >= titles.size() - 5000; i--) {
      const Title &title = titles[i];
      string words = title.title;
      while (!words.empty()) {
	string word = Normalize(Util::chop(words));
	worstwordcounts[word]++;
      }
      fprintf(out, "%d.  %.17g\t%s\t%s\n",
	      i,
	      title.cite_probability,
	      title.id.c_str(), title.title.c_str());
    }
    printf("Wrote %s.\n", worstfile.c_str());
    fclose(out);
  }

  vector<pair<string, int64>> wc;
  wc.reserve(worstwordcounts.size());
  for (const auto &p : worstwordcounts) {
    wc.push_back({p.first, p.second});
  }
  std::sort(
      wc.begin(), wc.end(),
      [](const pair<string, int64> &a,
	 const pair<string, int64> &b) {
	return a.second > b.second;
      });

  {
    FILE *out = fopen("worstpaperwords.txt", "wb");
    for (int i = 0; i < 1000 && i < wc.size(); i++) {
      fprintf(out, "%s\t%lld\n", wc[i].first.c_str(), wc[i].second);
    }
    printf("Also wrote worstpaperwords.txt\n");
    fclose(out);
  }
  
  return 0;
}
