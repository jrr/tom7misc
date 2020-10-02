
#include <optional>
#include <string>

struct ProcessUtil {

  // Run command and wait for it to terminate. Return all its stdout
  // in a string. Returns nullopt if the command can't be executed.
  static std::optional<std::string> GetOutput(const std::string &cmd);

  // TODO: Fancier versions with options, like:
  //  - after some timeout, kill the process
  //  - string to pass as stdin
  // .. and more return values like
  //  - exit status
  //  - stderr
};
