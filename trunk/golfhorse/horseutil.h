#ifndef __HORSEUTIL_H
#define __HORSEUTIL_H

#include <string>
#include <vector>

// Encode the word as a delta from the previous word.
// If the previous word is "boughs"
// then                    "bought"
// can be encoded as       "AAAAAB"
// where A means "character above +0" and B means "character above +1"
// all mod 26 of course.
// There are a number of options when the word is longer than the
// previous one (which is typical). We can just treat the previous
// word as being padded with a, which means that "run" -> "running"
// is encoded as "AAANING".
// And actually since encoding this way just uses A-Z, let's actually
// just use lowercase.
std::vector<std::string> DeltaEncode(
    const std::vector<std::string> &words);

std::string PrefixEncode(const std::vector<std::string> &words,
			 bool max_prefix_9,
			 bool suffix_encoding);

#endif
