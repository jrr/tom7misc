/* Extended string routines
 *
 * Copyright notice for this file:
 *  Copyright (C) 2004 Jason Oster (Parasyte)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "xstring.h"
#include <string>
#include <vector>

using namespace std;

// convert input string into vector of string tokens
//
// consecutive delimiters will be treated as single delimiter
// delimiters are _not_ included in return data
vector<string> TokenizeStr(const string &str, const string &delims) {
  // Skip delims at beginning, find start of first token
  string::size_type last_pos = str.find_first_not_of(delims, 0);
  // Find next delimiter @ end of token
  string::size_type pos = str.find_first_of(delims, last_pos);

  // output vector
  vector<string> tokens;

  while (string::npos != pos || string::npos != last_pos) {
    // Found a token, add it to the vector.
    tokens.push_back(str.substr(last_pos, pos - last_pos));
    // Skip delims.  Note the "not_of". this is beginning of token
    last_pos = str.find_first_not_of(delims, pos);
    // Find next delimiter at end of token.
    pos = str.find_first_of(delims, last_pos);
  }

  return tokens;
}
