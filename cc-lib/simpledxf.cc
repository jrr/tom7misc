
#include "simpledxf.h"

#include <cstdio>
#include <cstdint>
#include <string>
#include <vector>
#include <string_view>
#include <unordered_map>

#include "re2/re2.h"
#include "base/logging.h"
#include "util.h"

using namespace std;
using int64 = int64_t;
using StringPiece = re2::StringPiece;

SimpleDXF::ValueType SimpleDXF::CodeType(int code) {
  if (code >= 0 && code <= 9) return ValueType::STRING;
  if (code >= 10 && code <= 39) return ValueType::FLOAT; // "3D point value"
  if (code >= 40 && code <= 59) return ValueType::FLOAT;
  if (code >= 60 && code <= 79) return ValueType::INT;
  if (code >= 90 && code <= 99) return ValueType::INT;
  if (code == 100) return ValueType::STRING;
  if (code == 102) return ValueType::STRING;
  if (code == 105) return ValueType::STRING;
  if (code >= 110 && code <= 119) return ValueType::FLOAT;
  if (code >= 120 && code <= 129) return ValueType::FLOAT;
  if (code >= 130 && code <= 139) return ValueType::FLOAT;
  if (code >= 140 && code <= 149) return ValueType::FLOAT;
  if (code >= 160 && code <= 169) return ValueType::INT;  // 64-bit
  if (code >= 170 && code <= 179) return ValueType::INT;
  if (code >= 210 && code <= 239) return ValueType::FLOAT;
  if (code >= 270 && code <= 279) return ValueType::INT;
  if (code >= 280 && code <= 289) return ValueType::INT;
  if (code >= 290 && code <= 299) return ValueType::INT;  // Boolean
  if (code >= 300 && code <= 309) return ValueType::STRING;
  if (code >= 310 && code <= 319) return ValueType::STRING;
  if (code >= 320 && code <= 329) return ValueType::STRING;
  if (code >= 330 && code <= 369) return ValueType::STRING;
  if (code >= 370 && code <= 379) return ValueType::INT;
  if (code >= 380 && code <= 389) return ValueType::INT;
  if (code >= 390 && code <= 399) return ValueType::STRING;
  if (code >= 400 && code <= 409) return ValueType::INT;
  if (code >= 410 && code <= 419) return ValueType::STRING;
  if (code >= 420 && code <= 429) return ValueType::INT;
  if (code >= 430 && code <= 439) return ValueType::STRING;
  if (code >= 440 && code <= 449) return ValueType::INT;
  if (code >= 450 && code <= 459) return ValueType::INT;
  if (code >= 460 && code <= 469) return ValueType::FLOAT;
  if (code >= 470 && code <= 479) return ValueType::STRING;
  if (code >= 480 && code <= 481) return ValueType::STRING;
  if (code == 999) return ValueType::STRING;
  if (code >= 1000 && code <= 1009) return ValueType::STRING;
  if (code >= 1010 && code <= 1059) return ValueType::FLOAT;
  if (code >= 1060 && code <= 1070) return ValueType::INT;
  if (code == 1071) return ValueType::INT;

  return ValueType::INVALID;
}

#define SPACE_RE "[ \\t]"

vector<SimpleDXF::Field> SimpleDXF::GetFields(const string &contents) {
  RE2 code_re{SPACE_RE "*(-?[0-9]+)" SPACE_RE "*"};
  RE2 integer_re = {SPACE_RE "*(-?[0-9]+)" SPACE_RE "*"};
  RE2 float_re = {SPACE_RE "*(-?[0-9.]+)" SPACE_RE "*"};
  /* string_re is just the whole line. */
  
  vector<string> lines = Util::SplitToLines(contents);
  vector<Field> fields;
  fields.reserve(lines.size() >> 1);
  for (int i = 0; i < (int)lines.size() - 1; i += 2) {
    const string &code_line = lines[i];
    const string &value_line = lines[i + 1];
    int code = -999;
    CHECK(RE2::FullMatch(code_line, code_re, &code))
      << "Expected number on a line by itself: " << code_line;
    switch (CodeType(code)) {
    default:
    case ValueType::INVALID:
      LOG(FATAL) << "Unknown code " << code;
      break;
    case ValueType::INT: {
      int64 value = 0LL;
      CHECK(RE2::FullMatch(value_line, integer_re, &value))
	<< "Expected integer for code " << code << "; got:\n"
	<< value_line;
      fields.emplace_back(code, Value{value});
      break;
    }
    case ValueType::FLOAT: {
      double value = 0.0;
      CHECK(RE2::FullMatch(value_line, float_re, &value))
	<< "Expected float for code " << code << "; got:\n"
	<< value_line;
      fields.emplace_back(code, Value{value});
      break;
    }
    case ValueType::STRING: {
      fields.emplace_back(code, Value{value_line});
      break;
    }
    }
  }
  return fields;
}

string SimpleDXF::ValueString(const Value &value) {
  char buf[64] = {};
  switch (value.type) {
  case ValueType::INT:
    sprintf(buf, "%lld", value.i);
    return buf;
  case ValueType::FLOAT:
    sprintf(buf, "%.17g", value.d);
    return buf;
  case ValueType::STRING:
    return value.s;
  default:
    return "?!BAD_VALUE";
  }
}

// Get the first field of the named section (after 0 SECTION; 2 NAME).
// Returns -1 on failure.
static int GetSectionStart(const vector<SimpleDXF::Field> &fields,
			   const string_view &name) {
  for (int i = 0; i < (int)fields.size() - 1; i++) {
    if (fields[i].code == 0 &&
	fields[i + 1].code == 2 &&
	fields[i].value.s == "SECTION" &&
	fields[i + 1].value.s == name) {
      return i + 2;
    }
  }
  return -1;
}

vector<SimpleDXF::Entity> SimpleDXF::GetEntities(
    const vector<Field> &fields) {
  // TODO: We could do a high-level parse of the file into sections,
  // but just scanning for the ENTITIES section is easy enough.

  int start = GetSectionStart(fields, "ENTITIES");
  CHECK(start != -1) << "No ENTITIES section?";

  vector<Entity> entities;
  Entity *current = nullptr;
  for (int i = start; i < (int)fields.size(); i++) {
    if (fields[i].code == 0) {
      // Done!
      if (fields[i].value.s == "ENDSEC")
	return entities;

      // Otherwise, start a new entity.
      entities.push_back(Entity{fields[i].value.s, {}});
      current = &entities.back();
      current->fields[0].push_back(fields[i].value);
    } else {
      CHECK(current != nullptr) << "ENTITIES section doesn't start "
	"with an entity? (code 0)";
      current->fields[fields[i].code].push_back(fields[i].value);
    }
  }
  LOG(FATAL) << "ENTITIES section had no ENDSEC?";
  return {};
}
