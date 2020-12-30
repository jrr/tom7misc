#include "fontdb.h"

#include "stb_truetype.h"
#include "util.h"


using namespace std;

FontDB::FontDB() {
  std::unordered_map<string, Type> string_type;
  for (const Type t : {Type::SANS, Type::SERIF, Type::FANCY,
	Type::TECHNO, Type::DECORATIVE,
	Type::MESSY, Type::DINGBATS, Type::OTHER, Type::BROKEN,
	Type::UNKNOWN}) {
    string_type[TypeString(t)] = t;
  }
    
  for (string line : Util::ReadFileToLines(DATABASE_FILENAME)) {
    const string flagstring = Util::chop(line);
    const double diffscore = Util::ParseDouble(Util::chop(line), -1.0);
    const string typestring = Util::chop(line);
    const string filename = Util::losewhitel(line);

    auto it = string_type.find(typestring);
    CHECK(it != string_type.end()) << "Unknown type " << typestring;
    const Type type = it->second;
    CHECK(files.find(filename) == files.end()) <<
      "Duplicate in fontdb: " << filename;

    Info info;
    info.type = type;
    if (type != Type::UNKNOWN) num_sorted++;
    info.bitmap_diffs = diffscore;
    for (char c : flagstring) {
      if (c == '_') continue;
      auto [flag, on] = CharFlag(c);
      info.flags[flag] = on;
    }
    files[filename] = info;
  }
    
  printf("Total in FontDB: %lld\n", files.size());
}

void FontDB::Save() {
  {
    vector<string> lines;
    // XXX sort by filename
    for (const auto &[filename, info] : files) {
      lines.push_back(StringPrintf("%s %.5f %s %s",
				   FlagString(info.flags).c_str(),
				   info.bitmap_diffs,
				   TypeString(info.type),
				   filename.c_str()));
    }
    Util::WriteLinesToFile(DATABASE_FILENAME, lines);
    printf("Wrote %lld entries to %s\n",
	   (int64)lines.size(),
	   DATABASE_FILENAME);
  }
      
  {
    // Temporary? P/R curve export
    struct Labeled {
      // "0" = more likely to be same case, 1 = least likely.
      float score = 0.0;
      bool same_case = false;
      Labeled(float score, bool same_case) :
	score(score), same_case(same_case) {}
    };

    vector<Labeled> labs;
    for (const auto &[filename, info] : files) {
      if (info.bitmap_diffs >= 0.0 && info.bitmap_diffs <= 1.0) {
	auto it = info.flags.find(Flag::SAME_CASE);
	if (it != info.flags.end()) {
	  labs.emplace_back(info.bitmap_diffs, it->second);
	}
      }
    }

    std::sort(labs.begin(), labs.end(),
	      [](const Labeled &a, const Labeled &b) {
		return a.score < b.score;
	      });
      
    // "Positive" here means same case (this is a low score).
    //
    // As we go, assuming the threshold is set to the current value,
    // what would our P/R be? This means calling everything we've
    // already seen a positive and everything else a negative. So
    // first, a parallel array giving the number of true positives
    // for the rest of the array (strictly higher scores).
    vector<int64> remaining_positives(labs.size(), 0);
    int64 total_positives = 0;
    {
      int64 pos_above = 0;
      for (int64 i = labs.size() - 1; i >= 0; i--) {
	remaining_positives[i] = pos_above;
	if (labs[i].same_case) {
	  total_positives++;
	  pos_above++;
	}
      }
    }

    // Now compute precision at each threshold. Every item up to the
    // threshold being considered is predicted positive.
    int64 positives_so_far = 0;
    std::vector<string> lines;
    lines.reserve(labs.size() + 1);
    lines.push_back("threshold\t"
		    "recall\t"
		    "precision");
    for (int64 i = 0; i < labs.size(); i++) {
      if (labs[i].same_case) positives_so_far++;
      double precision = (double)positives_so_far / (i + 1);
      lines.push_back(
	  StringPrintf("%.5f\t%.5f\t%.5f",
		       labs[i].score,
		       (total_positives - remaining_positives[i]) /
		       (double)total_positives,
		       precision));
    }
    Util::WriteLinesToFile("pr-curve.tsv", lines);
  }

  dirty = false;
}
