#include "process-util.h"

#include <string>

#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;

int main(int argc, char **argv) {

  if (argc > 1) {
    string arg = argv[1];

    if (arg == "-child") {
      printf("i am child (:\n");
      return 0;
    } else if (arg == "-longchild") {
      for (int i = 0; i < 555; i++) {
	printf("%c", (i & 255));
      }
      return 0;
    }

  } else {

    {
      string cmd1 = StringPrintf("%s -child", argv[0]);
      printf("running self: [%s]\n", cmd1.c_str());
    
      std::optional<string> reso =
	ProcessUtil::GetOutput(cmd1);

      CHECK(reso.has_value());
      CHECK_EQ(reso.value(), "i am child (:\n");
    }

    {
      string cmd2 = StringPrintf("%s -longchild", argv[0]);
      printf("running self: [%s]\n", cmd2.c_str());
    
      std::optional<string> reso =
	ProcessUtil::GetOutput(cmd2);

      CHECK(reso.has_value());
      const string &res = reso.value();
      CHECK_EQ(res.size(), 555) << res.size() << "\n" << res;
      for (int i = 0; i < (int)res.size(); i++) {
	CHECK_EQ(res[i], i & 255) << i;
      }
    }
    
    printf("OK!\n");
    return 0;
  }
}

