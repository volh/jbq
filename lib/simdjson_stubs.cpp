#include <cstdlib>
#include <limits>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>

#include <simdjson.h>

extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

namespace {

constexpr tag_t TAG_VALUE_BOOL = 0;
constexpr tag_t TAG_VALUE_INT = 1;
constexpr tag_t TAG_VALUE_FLOAT = 2;
constexpr tag_t TAG_VALUE_STRING = 3;
constexpr tag_t TAG_VALUE_ARRAY = 4;
constexpr tag_t TAG_VALUE_OBJECT = 5;

[[noreturn]] void fail(const std::string &message) {
  caml_failwith((std::string("json: ") + message).c_str());
}

value alloc_value_bool(bool b) {
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc(1, TAG_VALUE_BOOL);
  Store_field(block, 0, Val_bool(b));
  CAMLreturn(block);
}

value alloc_value_int(intnat i) {
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc(1, TAG_VALUE_INT);
  Store_field(block, 0, Val_long(i));
  CAMLreturn(block);
}

value alloc_value_float(double f) {
  CAMLparam0();
  CAMLlocal2(block, boxed);
  boxed = caml_copy_double(f);
  block = caml_alloc(1, TAG_VALUE_FLOAT);
  Store_field(block, 0, boxed);
  CAMLreturn(block);
}

value alloc_value_string(std::string_view s) {
  CAMLparam0();
  CAMLlocal2(block, str);
  str = caml_alloc_initialized_string(s.size(), s.data());
  block = caml_alloc(1, TAG_VALUE_STRING);
  Store_field(block, 0, str);
  CAMLreturn(block);
}

value alloc_value_array(value list) {
  CAMLparam1(list);
  CAMLlocal1(block);
  block = caml_alloc(1, TAG_VALUE_ARRAY);
  Store_field(block, 0, list);
  CAMLreturn(block);
}

value alloc_value_object(value fields) {
  CAMLparam1(fields);
  CAMLlocal1(result);
  static const value *closure = nullptr;
  if (closure == nullptr) {
    closure = caml_named_value("jx_simdjson_object_of_fields");
    if (closure == nullptr) {
      fail("object constructor callback is not registered");
    }
  }
  result = caml_callback(*closure, fields);
  CAMLreturn(result);
}

value alloc_value_bigint(std::string_view s) {
  CAMLparam0();
  CAMLlocal2(arg, result);
  static const value *closure = nullptr;
  if (closure == nullptr) {
    closure = caml_named_value("jx_simdjson_bigint_value_of_string");
    if (closure == nullptr) {
      fail("bigint constructor callback is not registered");
    }
  }
  arg = caml_alloc_initialized_string(s.size(), s.data());
  result = caml_callback(*closure, arg);
  CAMLreturn(result);
}

template <typename JsonValue>
value convert_json_value(JsonValue &json_value);

template <typename JsonArray>
value convert_json_array(JsonArray &array) {
  CAMLparam0();
  CAMLlocal4(head, tail, cell, item);
  head = Val_emptylist;
  tail = Val_emptylist;
  bool first = true;

  for (auto elem_result : array) {
    simdjson::ondemand::value elem;
    auto error = elem_result.get(elem);
    if (error) {
      fail(std::string("failed to read array element: ") +
           simdjson::error_message(error));
    }

    item = convert_json_value(elem);
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, item);
    Store_field(cell, 1, Val_emptylist);
    if (first) {
      head = cell;
      first = false;
    } else {
      Store_field(tail, 1, cell);
    }
    tail = cell;
  }

  CAMLreturn(alloc_value_array(head));
}

template <typename JsonObject>
value convert_json_object(JsonObject &object) {
  CAMLparam0();
  CAMLlocal5(head, tail, cell, pair, item);
  head = Val_emptylist;
  tail = Val_emptylist;
  bool first = true;

  for (auto field : object) {
    std::string_view key;
    auto error = field.unescaped_key().get(key);
    if (error) {
      fail(std::string("failed to read object key: ") +
           simdjson::error_message(error));
    }

    auto field_value = field.value();
    item = convert_json_value(field_value);

    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, caml_alloc_initialized_string(key.size(), key.data()));
    Store_field(pair, 1, item);

    cell = caml_alloc(2, 0);
    Store_field(cell, 0, pair);
    Store_field(cell, 1, Val_emptylist);
    if (first) {
      head = cell;
      first = false;
    } else {
      Store_field(tail, 1, cell);
    }
    tail = cell;
  }

  CAMLreturn(alloc_value_object(head));
}

template <typename JsonValue>
value convert_json_number(JsonValue &json_value) {
  CAMLparam0();
  simdjson::ondemand::number_type number_type;
  auto error = json_value.get_number_type().get(number_type);
  if (error) {
    fail(std::string("failed to inspect number type: ") +
         simdjson::error_message(error));
  }

  switch (number_type) {
  case simdjson::ondemand::number_type::signed_integer: {
    int64_t i;
    error = json_value.get_int64().get(i);
    if (error) {
      fail(std::string("failed to decode signed integer: ") +
           simdjson::error_message(error));
    }
    if (i >= std::numeric_limits<intnat>::min() &&
        i <= std::numeric_limits<intnat>::max()) {
      CAMLreturn(alloc_value_int(static_cast<intnat>(i)));
    }
    CAMLreturn(alloc_value_bigint(std::to_string(i)));
  }
  case simdjson::ondemand::number_type::unsigned_integer: {
    uint64_t u;
    error = json_value.get_uint64().get(u);
    if (error) {
      fail(std::string("failed to decode unsigned integer: ") +
           simdjson::error_message(error));
    }
    if (u <= static_cast<uint64_t>(std::numeric_limits<intnat>::max())) {
      CAMLreturn(alloc_value_int(static_cast<intnat>(u)));
    }
    CAMLreturn(alloc_value_bigint(std::to_string(u)));
  }
  case simdjson::ondemand::number_type::floating_point_number: {
    double d;
    error = json_value.get_double().get(d);
    if (error) {
      fail(std::string("failed to decode floating number: ") +
           simdjson::error_message(error));
    }
    CAMLreturn(alloc_value_float(d));
  }
  case simdjson::ondemand::number_type::big_integer: {
    std::string_view raw = json_value.raw_json_token();
    CAMLreturn(alloc_value_bigint(raw));
  }
  }

  fail("unsupported number type");
}

template <typename JsonValue>
value convert_json_value(JsonValue &json_value) {
  CAMLparam0();
  simdjson::ondemand::json_type type;
  auto error = json_value.type().get(type);
  if (error) {
    fail(std::string("failed to inspect JSON type: ") +
         simdjson::error_message(error));
  }

  switch (type) {
  case simdjson::ondemand::json_type::array: {
    simdjson::ondemand::array array;
    error = json_value.get_array().get(array);
    if (error) {
      fail(std::string("failed to decode array: ") +
           simdjson::error_message(error));
    }
    CAMLreturn(convert_json_array(array));
  }
  case simdjson::ondemand::json_type::object: {
    simdjson::ondemand::object object;
    error = json_value.get_object().get(object);
    if (error) {
      fail(std::string("failed to decode object: ") +
           simdjson::error_message(error));
    }
    CAMLreturn(convert_json_object(object));
  }
  case simdjson::ondemand::json_type::number:
    CAMLreturn(convert_json_number(json_value));
  case simdjson::ondemand::json_type::string: {
    std::string_view s;
    error = json_value.get_string().get(s);
    if (error) {
      fail(std::string("failed to decode string: ") +
           simdjson::error_message(error));
    }
    CAMLreturn(alloc_value_string(s));
  }
  case simdjson::ondemand::json_type::boolean: {
    bool b;
    error = json_value.get_bool().get(b);
    if (error) {
      fail(std::string("failed to decode boolean: ") +
           simdjson::error_message(error));
    }
    CAMLreturn(alloc_value_bool(b));
  }
  case simdjson::ondemand::json_type::null:
    CAMLreturn(Val_int(0));
  case simdjson::ondemand::json_type::unknown:
    fail("unknown JSON type");
  }

  fail("unreachable JSON type");
}

struct top_array_stream {
  std::string json;
  simdjson::ondemand::parser parser;
  std::optional<simdjson::ondemand::document> document;
  std::optional<simdjson::ondemand::array> array;
  simdjson::ondemand::array_iterator iter{};
  simdjson::ondemand::array_iterator end{};

  explicit top_array_stream(const char *data, std::size_t len) : json(data, len) {
    simdjson::ondemand::document parsed_document;
    auto error = parser.iterate(json).get(parsed_document);
    if (error) {
      throw std::runtime_error(std::string("simdjson iterate failed: ") +
                               simdjson::error_message(error));
    }
    document.emplace(std::move(parsed_document));

    simdjson::ondemand::array parsed_array;
    error = document->get_array().get(parsed_array);
    if (error) {
      throw std::runtime_error(std::string("expected top-level JSON array: ") +
                               simdjson::error_message(error));
    }
    array.emplace(std::move(parsed_array));

    error = array->begin().get(iter);
    if (error) {
      throw std::runtime_error(std::string("failed to start array iteration: ") +
                               simdjson::error_message(error));
    }
    error = array->end().get(end);
    if (error) {
      throw std::runtime_error(std::string("failed to finish array iteration setup: ") +
                               simdjson::error_message(error));
    }
  }

  std::optional<std::string> next_raw() {
    if (iter == end) {
      return std::nullopt;
    }

    simdjson::ondemand::value value;
    auto error = (*iter).get(value);
    if (error) {
      throw std::runtime_error(std::string("failed to read array element: ") +
                               simdjson::error_message(error));
    }

    std::string_view raw;
    error = value.raw_json().get(raw);
    if (error) {
      throw std::runtime_error(std::string("failed to extract raw JSON element: ") +
                               simdjson::error_message(error));
    }

    ++iter;
    return std::string(raw);
  }

  value next_value() {
    CAMLparam0();
    CAMLlocal2(result, option);
    if (iter == end) {
      CAMLreturn(Val_none);
    }

    simdjson::ondemand::value elem;
    auto error = (*iter).get(elem);
    if (error) {
      fail(std::string("failed to read array element: ") +
           simdjson::error_message(error));
    }

    result = convert_json_value(elem);
    ++iter;

    option = caml_alloc(1, 0);
    Store_field(option, 0, result);
    CAMLreturn(option);
  }
};

top_array_stream *stream_val(value v) {
  return *reinterpret_cast<top_array_stream **>(Data_custom_val(v));
}

void finalize_stream(value v) {
  delete stream_val(v);
}

custom_operations stream_ops = {
    const_cast<char *>("jx.simdjson.top_array_stream"),
    finalize_stream,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default};

value alloc_stream(top_array_stream *stream) {
  value block = caml_alloc_custom(&stream_ops, sizeof(top_array_stream *), 0, 1);
  *reinterpret_cast<top_array_stream **>(Data_custom_val(block)) = stream;
  return block;
}

} // namespace

extern "C" CAMLprim value jx_simdjson_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}

extern "C" CAMLprim value jx_simdjson_version(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  result = caml_copy_string(SIMDJSON_VERSION);
  CAMLreturn(result);
}

extern "C" CAMLprim value jx_simdjson_top_array_stream_create(value json) {
  CAMLparam1(json);
  CAMLlocal1(result);
  try {
    auto *stream =
        new top_array_stream(String_val(json), caml_string_length(json));
    result = alloc_stream(stream);
    CAMLreturn(result);
  } catch (const std::exception &ex) {
    fail(ex.what());
  }
}

extern "C" CAMLprim value jx_simdjson_top_array_stream_next_raw(value stream) {
  CAMLparam1(stream);
  CAMLlocal2(result, payload);
  try {
    auto next = stream_val(stream)->next_raw();
    if (!next.has_value()) {
      CAMLreturn(Val_int(0));
    }
    payload = caml_copy_string(next->c_str());
    result = caml_alloc(1, 0);
    Store_field(result, 0, payload);
    CAMLreturn(result);
  } catch (const std::exception &ex) {
    fail(ex.what());
  }
}

extern "C" CAMLprim value jx_simdjson_top_array_stream_next_value(value stream) {
  CAMLparam1(stream);
  try {
    CAMLreturn(stream_val(stream)->next_value());
  } catch (const std::exception &ex) {
    fail(ex.what());
  }
}

extern "C" CAMLprim value jx_simdjson_parse_value(value json) {
  CAMLparam1(json);
  try {
    std::string source(String_val(json), caml_string_length(json));
    simdjson::ondemand::parser parser;
    simdjson::ondemand::document document;
    auto error = parser.iterate(source).get(document);
    if (error) {
      fail(std::string("simdjson iterate failed: ") +
           simdjson::error_message(error));
    }
    CAMLreturn(convert_json_value(document));
  } catch (const std::exception &ex) {
    fail(ex.what());
  }
}
