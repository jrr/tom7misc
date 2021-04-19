
#ifndef _ESCAPE_HTTPUTIL_H
#define _ESCAPE_HTTPUTIL_H

#include <string>

struct HTTPUtil {
  static std::string URLEncode(const std::string &);
};

enum class EntryType { ARG, FILE, };

// key/value in a form submission.
// Can be a simple 
struct FormEntry {
  EntryType ty;
  std::string name;
  std::string filename;
  std::string content;

  static FormEntry Arg(const std::string &name, const std::string &value) {
    return FormEntry(EntryType::ARG, name, "", value);
  }
  static FormEntry File(const std::string &name, const std::string &filename,
                        const std::string &content) {
    return FormEntry(EntryType::FILE, name, filename, content);
  }

private:
  FormEntry(EntryType ty, const std::string &name,
            const std::string &filename,
            const std::string &content) :
    ty(ty), name(name), filename(filename), content(content) {}
};

#endif
