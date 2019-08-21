// Parsing of simple DXF files. DXF is CAD format used by AutoCAD,
// for example.
// For best results, export in the oldest available format (e.g. R12).

// Format reference:
// images.autodesk.com/adsk/files/autocad_2012_pdf_dxf-reference_enu.pdf

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

struct SimpleDXF {
  enum class ValueType {
    INVALID,
    INT,
    FLOAT,
    STRING,
  };

  static ValueType CodeType(int code);

  struct Value {
    ValueType type;
    int64_t i = 0LL;
    double d = 0.0;
    std::string s;

    explicit Value(int64_t i) : type(ValueType::INT), i(i) {}
    explicit Value(double d) : type(ValueType::FLOAT), d(d) {}
    explicit Value(const std::string &s) : type(ValueType::STRING), s(s) {}
  };

  struct Field {
    int code = 0;
    Value value;

    Field(int code, Value value) : code(code), value(value) {}
  };

  // Read the fields from the file. The values are parsed according to
  // to the code's expected type, but not interpreted further.
  static std::vector<Field> GetFields(const std::string &contents);

  // Represents the full precision of finite doubles.
  static std::string ValueString(const Value &value);


  struct Entity {
    // LINE, CIRCLE, etc. Also appears as code 0 below.
    std::string type;
    // Collated by code, with order preserved. The values will all
    // have the same type, dictated by the code.
    std::unordered_map<int, std::vector<Value>> fields;
  };

  // Get the entities from the ENTITIES section, without any
  // interpretation.
  static std::vector<Entity> GetEntities(
      const std::vector<Field> &fields);
  
};
