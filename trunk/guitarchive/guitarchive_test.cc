#include <string>

#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"

#include "guitarchive.h"

using namespace std;

int main (int argc, char **argv) {
  CHECK("/c/temp/file.txt" ==
	Guitarchive::Frontslash("c:\\temp\\file.txt"));

  // TODO: Expose filename+contents -> Entry method, test basics.

  
  RE2 splitheader{// "\n*"
		  // Don't care about preserving the two lines here,
		  // since we discard this header anyway.
                  "(Received): (from .+)\n"
		  "[ \t]+.+\n"};

  string cont = 
    "Received: from animal-farm.nevada.edu by redrock.nevada.edu (5.65c/M1.4)\n"
    "	with SMTP id <AA22995>; Mon, 12 Apr 1993 10:39:03 -0700\n";

  re2::StringPiece tmp = cont;
  string key, value;

  CHECK(RE2::Consume(&tmp, splitheader, &key, &value));
  
  /*TODO ??
"Received: from madrone.eecs.ucdavis.edu by animal-farm.nevada.edu id <AA03540@animal-farm.nevada.edu>; Mon, 12 Apr 1993 10:39:00 -0700\n"
"Received: by madrone.eecs.ucdavis.edu (5.57/Ultrix3.0-C/eecs 1.1)\n"
"	id AA21762; Mon, 12 Apr 93 10:38:58 -0700\n"
  */
  return 0;
}
