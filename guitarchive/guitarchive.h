
#include <vector>
#include <string>
#include <string_view>


using namespace std;

// Parsed entry from disk. This is not sufficient to exactly recreate
// the file; see clean.cc for round-trip stuff.
struct Entry {
  // These are removed from the headers if present.
  string artist;
  string title;
  string album;
  // Other headers as key, value.
  vector<pair<string, string>> headers;

  string filename;
  
  // Unparsed body.
  string body;
};

struct Guitarchive {
  static void AddAllFilesRec(const string &dir, vector<string> *all_files);
  
  static string Frontslash(const string &s);
  static string Backslash(const string &s);

  static bool TryStripSuffix(string_view suffix, string_view *s);

  // Load everything, with headers.
  static vector<Entry> Load(int threads = 16);
};
