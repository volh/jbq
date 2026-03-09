let () =
  Jx.Interpreter.dispatch_ref := Jx.Stdlib_fns.dispatch;
  Jx.Value.xd_run_ref := Jx.Transducer.run

let eval_str query json_str =
  let input = Jx.Simdjson_native.parse_value json_str in
  let ast = Jx.Parser.parse query in
  let result = Jx.Interpreter.eval [] input ast in
  Jx.Printer.to_json ~compact:true result

let eval_pretty_str query json_str =
  let input = Jx.Simdjson_native.parse_value json_str in
  let ast = Jx.Parser.parse query in
  let result = Jx.Interpreter.eval [] input ast in
  Jx.Printer.to_json result

let eval_stream_str query json_str =
  let input = Jx.Simdjson_stream.top_array_input json_str in
  let ast = Jx.Parser.parse query in
  let result = Jx.Interpreter.eval [] input ast in
  Jx.Printer.to_json ~compact:true result

let test_simdjson_available () =
  Alcotest.(check bool) "simdjson available" true (Jx.Simdjson_native.available ())

let test_simdjson_version () =
  Alcotest.(check bool) "simdjson version non-empty" true
    (String.length (Jx.Simdjson_native.version ()) > 0)

let test_simdjson_top_array_elements_raw () =
  Alcotest.(check (list string))
    "top array raw elements"
    [ {|{"a":1}|}; {|[2,3]|}; {|4|} ]
    (Jx.Simdjson_native.top_array_elements_raw {|[{"a":1},[2,3],4]|})

let test_simdjson_top_array_rejects_non_array () =
  Alcotest.(check bool) "top array rejects non-array" true
    (try
       ignore (Jx.Simdjson_native.top_array_elements_raw {|{"a":1}|});
       false
     with Failure _ -> true)

let test_simdjson_top_array_value_seq () =
  let values =
    Jx.Simdjson_stream.top_array_value_seq {|[{"a":1},[2,3],4]|}
    |> List.of_seq
    |> List.map (Jx.Printer.to_json ~compact:true)
  in
  Alcotest.(check (list string))
    "top array value seq"
    [ {|{"a":1}|}; "[2,3]"; "4" ]
    values

let test_simdjson_parse_value () =
  Alcotest.(check string)
    "parse value nested"
    {|{"a":[1,2,{"b":true}],"c":null,"d":"x"}|}
    (Jx.Simdjson_native.parse_value
       {|{"a":[1,2,{"b":true}],"c":null,"d":"x"}|}
    |> Jx.Printer.to_json ~compact:true)

let test_simdjson_stream_where_take () =
  Alcotest.(check string)
    "streamed where | take"
    {|[{"name":"B","price":200}]|}
    (eval_stream_str "where .price > 100 | take 1"
       {|[{"name":"A","price":50},{"name":"B","price":200}]|})

let test_simdjson_stream_count () =
  Alcotest.(check string)
    "streamed count"
    "3"
    (eval_stream_str "count" "[1,2,3]")

let test_pretty_print_array_of_objects () =
  Alcotest.(check string)
    "pretty print array of objects"
    {|[ { "name": "Alice", "age": 30 }, { "name": "Bob", "age": 40 } ]|}
    (eval_pretty_str "map {name, age}"
       {|[{"name":"Alice","age":30},{"name":"Bob","age":40}]|})

let test_simdjson_stream_map () =
  Alcotest.(check string)
    "streamed map"
    {|["A","B"]|}
    (eval_stream_str "map .name" {|[{"name":"A"},{"name":"B"}]|})

let test_simdjson_stream_take () =
  Alcotest.(check string)
    "streamed take"
    "[1,2]"
    (eval_stream_str "take 2" "[1,2,3]")

let test_simdjson_stream_skip () =
  Alcotest.(check string)
    "streamed skip"
    "[3]"
    (eval_stream_str "skip 2" "[1,2,3]")

let test_simdjson_stream_unique () =
  Alcotest.(check string)
    "streamed unique"
    "[1,2,3]"
    (eval_stream_str "unique" "[1,2,1,3,2]")

let test_simdjson_stream_sort_by () =
  Alcotest.(check string)
    "streamed sort_by"
    {|[{"n":"A","p":10},{"n":"B","p":20}]|}
    (eval_stream_str "sort_by .p"
       {|[{"n":"B","p":20},{"n":"A","p":10}]|})

let test_simdjson_stream_group_by () =
  Alcotest.(check string)
    "streamed group_by"
    {|{"a":[{"t":"a","v":1},{"t":"a","v":2}],"b":[{"t":"b","v":3}]}|}
    (eval_stream_str "group_by .t"
       {|[{"t":"a","v":1},{"t":"b","v":3},{"t":"a","v":2}]|})

let test_simdjson_stream_reverse () =
  Alcotest.(check string)
    "streamed reverse"
    "[3,2,1]"
    (eval_stream_str "reverse" "[1,2,3]")

let test_simdjson_stream_first () =
  Alcotest.(check string)
    "streamed first"
    "1"
    (eval_stream_str "first" "[1,2,3]")

let test_simdjson_stream_last () =
  Alcotest.(check string)
    "streamed last"
    "3"
    (eval_stream_str "last" "[1,2,3]")

let test_simdjson_stream_where_map_count () =
  Alcotest.(check string)
    "streamed where | map | count"
    "2"
    (eval_stream_str "where .price > 100 | map .name | count"
       {|[{"name":"A","price":50},{"name":"B","price":200},{"name":"C","price":300}]|})

let test_identity () =
  Alcotest.(check string) "identity" "42" (eval_str "." "42")

let test_field_access () =
  Alcotest.(check string)
    "field access" "\"Alice\""
    (eval_str ".name" {|{"name": "Alice", "age": 30}|})

let test_nested_field () =
  Alcotest.(check string)
    "nested field" "\"NYC\""
    (eval_str ".user.address.city"
       {|{"user": {"address": {"city": "NYC"}}}|})

let test_pipe () =
  Alcotest.(check string)
    "pipe" "\"alice\""
    (eval_str ".name | lower" {|{"name": "Alice"}|})

let test_object_punning () =
  Alcotest.(check string)
    "object punning" {|{"name":"Alice","age":30}|}
    (eval_str "{name, age}" {|{"name": "Alice", "age": 30, "email": "a@b.com"}|})

let test_where () =
  Alcotest.(check string)
    "where filter" {|[{"name":"B","price":200}]|}
    (eval_str "where .price > 100"
       {|[{"name": "A", "price": 50}, {"name": "B", "price": 200}]|})

let test_map () =
  Alcotest.(check string)
    "map" {|["A","B"]|}
    (eval_str "map .name" {|[{"name": "A"}, {"name": "B"}]|})

let test_where_map_pipe () =
  Alcotest.(check string)
    "where | map pipe" {|["B"]|}
    (eval_str "where .price > 100 | map .name"
       {|[{"name": "A", "price": 50}, {"name": "B", "price": 200}]|})

let test_object_construct () =
  Alcotest.(check string)
    "object construct with explicit" {|[{"name":"B","expensive":true}]|}
    (eval_str {|where .price > 100 | map {name, expensive: .price > 100}|}
       {|[{"name": "A", "price": 50}, {"name": "B", "price": 200}]|})

let test_arithmetic () =
  Alcotest.(check string)
    "arithmetic" "7"
    (eval_str ".a + .b" {|{"a": 3, "b": 4}|})

let test_subtraction_associativity () =
  Alcotest.(check string)
    "subtraction is left associative" "5"
    (eval_str "10 - 3 - 2" "null")

let test_division_associativity () =
  Alcotest.(check string)
    "division is left associative" "2"
    (eval_str "20 / 5 / 2" "null")

let test_modulo_associativity () =
  Alcotest.(check string)
    "modulo is left associative" "0"
    (eval_str "8 % 3 % 2" "null")

let test_concat_associativity () =
  Alcotest.(check string)
    "concat is left associative" "\"a12\""
    (eval_str {|"a" ++ 1 ++ 2|} "null")

let test_operator_precedence () =
  Alcotest.(check string)
    "multiplication binds tighter than addition" "7"
    (eval_str "1 + 2 * 3" "null")

let test_string_concat () =
  Alcotest.(check string)
    "string concat" "\"hello world\""
    (eval_str {|.first ++ " " ++ .last|}
       {|{"first": "hello", "last": "world"}|})

let test_null_coalesce () =
  Alcotest.(check string)
    "null coalesce" "\"default\""
    (eval_str {|.missing? ?? "default"|} {|{"name": "Alice"}|})

let test_conditional () =
  Alcotest.(check string)
    "conditional" "\"expensive\""
    (eval_str {|if .price > 100 then "expensive" else "cheap"|}
       {|{"price": 200}|})

let test_let_binding () =
  Alcotest.(check string)
    "let binding" {|{"name":"Alice","total":150}|}
    (eval_str "let total = .price * .qty in {name, total}"
       {|{"name": "Alice", "price": 50, "qty": 3}|})

let test_sort_by () =
  Alcotest.(check string)
    "sort_by" {|[{"n":"A","p":10},{"n":"B","p":20}]|}
    (eval_str "sort_by .p"
       {|[{"n": "B", "p": 20}, {"n": "A", "p": 10}]|})

let test_take () =
  Alcotest.(check string) "take" "[1,2,3]" (eval_str "take 3" "[1,2,3,4,5]")

let test_skip () =
  Alcotest.(check string) "skip" "[4,5]" (eval_str "skip 3" "[1,2,3,4,5]")

let test_sum () =
  Alcotest.(check string) "sum" "15" (eval_str "sum" "[1,2,3,4,5]")

let test_bigint_parse_exact () =
  Alcotest.(check string)
    "bigint parse exact"
    "9223372036854775808"
    (eval_str ".n" {|{"n":9223372036854775808}|})

let test_huge_bigint_parse_exact () =
  Alcotest.(check string)
    "huge bigint parse exact"
    "1844674407370955161612345"
    (eval_str ".n" {|{"n":1844674407370955161612345}|})

let test_negative_bigint_parse_exact () =
  Alcotest.(check string)
    "negative bigint parse exact"
    "-9223372036854775809"
    (eval_str ".n" {|{"n":-9223372036854775809}|})

let test_bigint_query_add_exact () =
  Alcotest.(check string)
    "bigint query add exact"
    "9223372036854775809"
    (eval_str "9223372036854775808 + 1" "null")

let test_bigint_query_mul_exact () =
  Alcotest.(check string)
    "bigint query mul exact"
    "92233720368547758080"
    (eval_str "9223372036854775808 * 10" "null")

let test_bigint_sum_exact () =
  Alcotest.(check string)
    "bigint sum exact"
    "9223372036854775809"
    (eval_str "sum" "[9223372036854775808,1]")

let test_bigint_distinct_eq () =
  Alcotest.(check string)
    "bigint equality stays distinct"
    "false"
    (eval_str "9223372036854775808 == 9223372036854775809" "null")

let test_keys () =
  Alcotest.(check string)
    "keys preserves order" {|["b","a"]|}
    (eval_str "keys" {|{"b": 1, "a": 2}|})

let test_count () =
  Alcotest.(check string) "count" "3" (eval_str "count" "[1,2,3]")

let test_unique () =
  Alcotest.(check string)
    "unique preserves order" "[3,1,2]"
    (eval_str "unique" "[3,1,2,1,3,2]")

let test_unique_objects () =
  Alcotest.(check string)
    "unique deduplicates structural objects"
    {|[{"a":1},{"a":2}]|}
    (eval_str "unique" {|[{"a":1},{"a":1},{"a":2}]|})

let test_optional_field () =
  Alcotest.(check string)
    "optional field" "null"
    (eval_str ".missing?" {|{"name": "Alice"}|})

let test_unicode_and_escaped_keys () =
  let musical = "\240\157\132\158" in
  Alcotest.(check string)
    "unicode and escaped keys"
    (Printf.sprintf
       {|{"s":"hello\nworld","k\"y":1,"u":"Привіт","e":"é","m":"%s"}|} musical)
    (eval_str "."
       {|{"s":"hello\nworld","k\"y":1,"u":"Привіт","e":"é","m":"\uD834\uDD1E"}|})

let test_malformed_json_errors () =
  Alcotest.(check bool) "malformed json errors" true
    (try
       ignore (Jx.Simdjson_native.parse_value {|{"a":|});
       false
     with Failure _ -> true)

let test_deep_array_roundtrip () =
  let rec build n acc = if n = 0 then acc else build (n - 1) ("[" ^ acc ^ "]") in
  let json = build 200 "[]" in
  Alcotest.(check string) "deep array roundtrip" json (eval_str "." json)

let test_deep_object_roundtrip () =
  let rec build n acc =
    if n = 0 then acc else build (n - 1) ("{\"k\":" ^ acc ^ "}")
  in
  let json = build 200 "{}" in
  Alcotest.(check string) "deep object roundtrip" json (eval_str "." json)

let test_strict_null () =
  Alcotest.(check bool) "strict null errors" true
    (try
       ignore (eval_str ".missing" {|{"name": "Alice"}|});
       false
     with Jx.Error.Jx_error _ -> true)

let test_range () =
  Alcotest.(check string) "range" "[0,1,2,3,4]" (eval_str "range 5" "null")

let test_group_by () =
  Alcotest.(check string)
    "group_by returns object"
    {|{"a":[{"t":"a","v":1},{"t":"a","v":2}],"b":[{"t":"b","v":3}]}|}
    (eval_str "group_by .t"
       {|[{"t":"a","v":1},{"t":"b","v":3},{"t":"a","v":2}]|})

let test_pick_space_separated_args () =
  Alcotest.(check string)
    "pick supports space separated dotted args"
    {|{"name":"Alice","email":"a@b.com"}|}
    (eval_str "pick .name .email"
       {|{"name":"Alice","email":"a@b.com","age":30}|})

let test_pick_semicolon_args () =
  Alcotest.(check string)
    "pick supports semicolon separated field args"
    {|{"name":"Alice","email":"a@b.com"}|}
    (eval_str "pick .name; .email"
       {|{"name":"Alice","email":"a@b.com","age":30}|})

let test_spaced_postfix_rejected () =
  Alcotest.(check bool) "spaced postfix does not chain" true
    (try
       ignore (eval_str ".items [0]" {|{"items":[1,2,3]}|});
       false
     with Jx.Error.Jx_error _ -> true)

let test_avg () =
  let result = eval_str "avg" "[1,2,3,4,5]" in
  Alcotest.(check bool) "avg is 3.0" true
    (float_of_string result = 3.0)

let test_interpolation () =
  Alcotest.(check string)
    "string interpolation" "\"hello Alice, age 30\""
    (eval_str {|"hello ${.name}, age ${.age}"|}
       {|{"name": "Alice", "age": 30}|})

let test_interpolation_expr () =
  Alcotest.(check string)
    "interpolation with expression" "\"total: 150\""
    (eval_str {|"total: ${.price * .qty}"|}
       {|{"price": 50, "qty": 3}|})

let test_plain_string () =
  Alcotest.(check string)
    "plain string no interpolation" "\"hello world\""
    (eval_str {|"hello world"|} "null")

let test_truncate () =
  Alcotest.(check string)
    "truncate" "\"hel\""
    (eval_str ".name | truncate 3" {|{"name": "hello"}|})

let test_array_index () =
  Alcotest.(check string) "array index" "2" (eval_str ".[1]" "[1,2,3]")

let test_negative_index () =
  Alcotest.(check string)
    "negative index" "3"
    (eval_str ".[-1]" "[1,2,3]")

let test_slice () =
  Alcotest.(check string) "slice" "[2,3]" (eval_str ".[1:3]" "[1,2,3,4]")

(* === Lazy sequence tests === *)

let test_infinite_range_take () =
  Alcotest.(check string)
    "infinite range | take" "[0,1,2,3,4]"
    (eval_str "range | take 5" "null")

let test_infinite_range_where_take () =
  Alcotest.(check string)
    "infinite range | where | take" "[11,12,13,14,15]"
    (eval_str "range | where (. > 10) | take 5" "null")

let test_infinite_map_where_take () =
  Alcotest.(check string)
    "infinite range | map | where | take" "[64,81,100]"
    (eval_str "range | map (. * .) | where (. > 50) | take 3" "null")

let test_infinite_skip_take () =
  Alcotest.(check string)
    "infinite range | skip | take" "[5,6,7]"
    (eval_str "range | skip 5 | take 3" "null")

let test_infinite_first () =
  Alcotest.(check string)
    "infinite range | where | first" "0"
    (eval_str "range | where (. % 2 == 0) | first" "null")

let test_infinite_sum () =
  Alcotest.(check string)
    "infinite range | take | sum" "10"
    (eval_str "range | take 5 | sum" "null")

let test_infinite_count () =
  Alcotest.(check string)
    "infinite range | take | count" "100"
    (eval_str "range | take 100 | count" "null")

let test_infinite_where_skip_take () =
  Alcotest.(check string)
    "infinite multiples of 7 skipping first 5" "[35,42,49]"
    (eval_str "range | where (. % 7 == 0) | skip 5 | take 3" "null")

let test_flatmap_basic () =
  Alcotest.(check string)
    "flatmap flattens mapped arrays"
    {|[1,2,3,4]|}
    (eval_str "flatmap .items"
       {|[{"items":[1,2]},{"items":[3,4]}]|})

let test_flatmap_nested_drill () =
  Alcotest.(check string)
    "flatmap drills through nested arrays"
    {|["W","G","D"]|}
    (eval_str "flatmap .orders | flatmap .items | map .name"
       {|[{"orders":[{"items":[{"name":"W"},{"name":"G"}]}]},{"orders":[{"items":[{"name":"D"}]}]}]|})

let test_flatmap_with_where () =
  Alcotest.(check string)
    "flatmap composes with where"
    {|[3,4]|}
    (eval_str "flatmap .items | where (. > 2)"
       {|[{"items":[1,2]},{"items":[3,4]}]|})

let test_flatmap_with_take () =
  Alcotest.(check string)
    "flatmap composes with take for early exit"
    {|[1,2]|}
    (eval_str "flatmap .items | take 2"
       {|[{"items":[1,2]},{"items":[3,4]}]|})

let test_flatmap_with_lambda () =
  Alcotest.(check string)
    "flatmap with lambda"
    {|[1,1,2,2]|}
    (eval_str "flatmap (x => [x, x])" "[1,2]")

let test_flatmap_stream () =
  Alcotest.(check string)
    "flatmap over streamed input"
    {|[1,2,3,4]|}
    (eval_stream_str "flatmap .items"
       {|[{"items":[1,2]},{"items":[3,4]}]|})

let test_lazy_preserves_array_semantics () =
  Alcotest.(check string)
    "array input still returns array" "[2,4]"
    (eval_str "where (. % 2 == 0)" "[1,2,3,4,5]")

let test_infinite_map_interpolation () =
  Alcotest.(check string)
    "infinite range with string interpolation"
    {|["n=0","n=1","n=2"]|}
    (eval_str {|range | map "n=${.}" | take 3|} "null")

let test_lazy_no_realization () =
  (* Proves laziness: this would hang or OOM if realized eagerly *)
  Alcotest.(check bool)
    "infinite pipeline completes (proves laziness)" true
    (let result = eval_str "range | where (. > 1000000) | take 1" "null" in
     result = "[1000001]")

let () =
  Alcotest.run "jx"
    [
      ( "simdjson",
        [
          Alcotest.test_case "available" `Quick test_simdjson_available;
          Alcotest.test_case "version" `Quick test_simdjson_version;
          Alcotest.test_case "top array elements raw" `Quick test_simdjson_top_array_elements_raw;
          Alcotest.test_case "top array rejects non-array" `Quick test_simdjson_top_array_rejects_non_array;
          Alcotest.test_case "top array value seq" `Quick test_simdjson_top_array_value_seq;
          Alcotest.test_case "parse value" `Quick test_simdjson_parse_value;
          Alcotest.test_case "stream where take" `Quick test_simdjson_stream_where_take;
          Alcotest.test_case "stream count" `Quick test_simdjson_stream_count;
          Alcotest.test_case "pretty print array of objects" `Quick test_pretty_print_array_of_objects;
          Alcotest.test_case "stream map" `Quick test_simdjson_stream_map;
          Alcotest.test_case "stream take" `Quick test_simdjson_stream_take;
          Alcotest.test_case "stream skip" `Quick test_simdjson_stream_skip;
          Alcotest.test_case "stream unique" `Quick test_simdjson_stream_unique;
          Alcotest.test_case "stream sort_by" `Quick test_simdjson_stream_sort_by;
          Alcotest.test_case "stream group_by" `Quick test_simdjson_stream_group_by;
          Alcotest.test_case "stream reverse" `Quick test_simdjson_stream_reverse;
          Alcotest.test_case "stream first" `Quick test_simdjson_stream_first;
          Alcotest.test_case "stream last" `Quick test_simdjson_stream_last;
          Alcotest.test_case "stream where map count" `Quick test_simdjson_stream_where_map_count;
          Alcotest.test_case "stream flatmap" `Quick test_flatmap_stream;
        ] );
      ( "core",
        [
          Alcotest.test_case "identity" `Quick test_identity;
          Alcotest.test_case "field access" `Quick test_field_access;
          Alcotest.test_case "nested field" `Quick test_nested_field;
          Alcotest.test_case "pipe" `Quick test_pipe;
          Alcotest.test_case "arithmetic" `Quick test_arithmetic;
          Alcotest.test_case "subtraction associativity" `Quick test_subtraction_associativity;
          Alcotest.test_case "division associativity" `Quick test_division_associativity;
          Alcotest.test_case "modulo associativity" `Quick test_modulo_associativity;
          Alcotest.test_case "concat associativity" `Quick test_concat_associativity;
          Alcotest.test_case "operator precedence" `Quick test_operator_precedence;
          Alcotest.test_case "string concat" `Quick test_string_concat;
          Alcotest.test_case "null coalesce" `Quick test_null_coalesce;
          Alcotest.test_case "conditional" `Quick test_conditional;
          Alcotest.test_case "let binding" `Quick test_let_binding;
          Alcotest.test_case "array index" `Quick test_array_index;
          Alcotest.test_case "negative index" `Quick test_negative_index;
          Alcotest.test_case "slice" `Quick test_slice;
          Alcotest.test_case "bigint parse exact" `Quick test_bigint_parse_exact;
          Alcotest.test_case "huge bigint parse exact" `Quick test_huge_bigint_parse_exact;
          Alcotest.test_case "negative bigint parse exact" `Quick test_negative_bigint_parse_exact;
          Alcotest.test_case "bigint query add" `Quick test_bigint_query_add_exact;
          Alcotest.test_case "bigint query mul" `Quick test_bigint_query_mul_exact;
          Alcotest.test_case "bigint sum" `Quick test_bigint_sum_exact;
          Alcotest.test_case "bigint distinct eq" `Quick test_bigint_distinct_eq;
        ] );
      ( "objects",
        [
          Alcotest.test_case "punning" `Quick test_object_punning;
          Alcotest.test_case "construct" `Quick test_object_construct;
        ] );
      ( "collections",
        [
          Alcotest.test_case "where" `Quick test_where;
          Alcotest.test_case "map" `Quick test_map;
          Alcotest.test_case "where | map" `Quick test_where_map_pipe;
          Alcotest.test_case "sort_by" `Quick test_sort_by;
          Alcotest.test_case "group_by" `Quick test_group_by;
          Alcotest.test_case "pick spaced args" `Quick test_pick_space_separated_args;
          Alcotest.test_case "pick semicolon args" `Quick test_pick_semicolon_args;
          Alcotest.test_case "spaced postfix rejected" `Quick test_spaced_postfix_rejected;
          Alcotest.test_case "unique" `Quick test_unique;
          Alcotest.test_case "unique objects" `Quick test_unique_objects;
          Alcotest.test_case "flatmap" `Quick test_flatmap_basic;
          Alcotest.test_case "flatmap nested drill" `Quick test_flatmap_nested_drill;
          Alcotest.test_case "flatmap with where" `Quick test_flatmap_with_where;
          Alcotest.test_case "flatmap with take" `Quick test_flatmap_with_take;
          Alcotest.test_case "flatmap with lambda" `Quick test_flatmap_with_lambda;
          Alcotest.test_case "take" `Quick test_take;
          Alcotest.test_case "skip" `Quick test_skip;
          Alcotest.test_case "count" `Quick test_count;
          Alcotest.test_case "range" `Quick test_range;
          Alcotest.test_case "keys" `Quick test_keys;
        ] );
      ( "aggregation",
        [
          Alcotest.test_case "sum" `Quick test_sum;
          Alcotest.test_case "avg" `Quick test_avg;
        ] );
      ( "strings",
        [
          Alcotest.test_case "truncate" `Quick test_truncate;
          Alcotest.test_case "interpolation" `Quick test_interpolation;
          Alcotest.test_case "interpolation expr" `Quick test_interpolation_expr;
          Alcotest.test_case "plain string" `Quick test_plain_string;
        ] );
      ( "null handling",
        [
          Alcotest.test_case "optional field" `Quick test_optional_field;
          Alcotest.test_case "strict null" `Quick test_strict_null;
        ] );
      ( "json parser",
        [
          Alcotest.test_case "unicode and escaped keys" `Quick test_unicode_and_escaped_keys;
          Alcotest.test_case "malformed json" `Quick test_malformed_json_errors;
          Alcotest.test_case "deep array roundtrip" `Quick test_deep_array_roundtrip;
          Alcotest.test_case "deep object roundtrip" `Quick test_deep_object_roundtrip;
        ] );
      ( "lazy sequences",
        [
          Alcotest.test_case "infinite range | take" `Quick test_infinite_range_take;
          Alcotest.test_case "infinite range | where | take" `Quick test_infinite_range_where_take;
          Alcotest.test_case "infinite map | where | take" `Quick test_infinite_map_where_take;
          Alcotest.test_case "infinite skip | take" `Quick test_infinite_skip_take;
          Alcotest.test_case "infinite first" `Quick test_infinite_first;
          Alcotest.test_case "infinite sum" `Quick test_infinite_sum;
          Alcotest.test_case "infinite count" `Quick test_infinite_count;
          Alcotest.test_case "infinite where | skip | take" `Quick test_infinite_where_skip_take;
          Alcotest.test_case "array semantics preserved" `Quick test_lazy_preserves_array_semantics;
          Alcotest.test_case "map with interpolation" `Quick test_infinite_map_interpolation;
          Alcotest.test_case "laziness proof (1M skip)" `Quick test_lazy_no_realization;
        ] );
    ]
