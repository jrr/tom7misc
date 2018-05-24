
#include "rapidjson/document.h"

#include "base/stringprintf.h"
#include "base/logging.h"

// Test of rapidjson library, and a simple example.
static void TestRapid() {
  using namespace rapidjson;
  Document document;
  CHECK(!document.Parse(R"({ "a": 5, "b": "hello", "c": [3, 4] })").HasParseError());
  CHECK(document.IsObject());
  CHECK(document.HasMember("a"));
  CHECK(document.HasMember("b"));
  CHECK(!document.HasMember("no"));
  CHECK_EQ("hello", (string)document["b"].GetString());
  CHECK_EQ((string)"hello", document["b"].GetString());
  CHECK(document["a"].IsNumber());
  CHECK(document["a"].IsInt());
  CHECK_EQ(5, document["a"].GetInt());

  const Value &c = document["c"];
  CHECK(c.IsArray());
  CHECK_EQ(2, c.Size());
  for (const Value &v : c.GetArray()) {
    CHECK(v.IsInt());
    printf("%d\n", v.GetInt());
  }
}

int main(int argc, char **argv) {
  TestRapid();
  return 0;
}
