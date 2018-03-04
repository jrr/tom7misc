
#include "taojson/json.hpp"
#include "base/stringprintf.h"
#include "base/logging.h"

// Test of taocpp json library, and a simple example.

using namespace tao;

int main(int argc, char **argv) {

  json::value v = json::from_string( R"({ "a": 5, "b": "hello" })" );

  printf("Pretty:\n%s\n", json::to_string(v).c_str());

  CHECK(v.type() == json::type::OBJECT);
  CHECK(v.is_object());
  CHECK(v.get<json::type::OBJECT>()["a"] == 5);
  CHECK(v.get_object()["b"] == "hello");

  return 0;
}
