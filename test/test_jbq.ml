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

let eval_error_kind query json_str =
  try
    ignore (eval_str query json_str);
    None
  with Jx.Error.Jx_error err -> Some err.kind

let test_cli_args_resolve_missing_query () =
  Alcotest.(check bool) "missing query rejected" true
    (match Jx.Cli_args.resolve ~query:None ~input_file:None ~input_opt:None ~schema:false with
     | Error Jx.Cli_args.Missing_query -> true
     | _ -> false)

let test_cli_args_resolve_schema_file_flag () =
  Alcotest.(check (option string)) "schema file flag becomes input source"
    (Some "data.json")
    (match
       Jx.Cli_args.resolve ~query:None ~input_file:None
         ~input_opt:(Some "data.json") ~schema:true
     with
    | Ok resolved -> resolved.input_source
    | Error _ -> None)

let test_cli_args_resolve_duplicate_input () =
  Alcotest.(check bool) "duplicate input rejected" true
    (match
       Jx.Cli_args.resolve ~query:(Some ".") ~input_file:(Some "a.json")
         ~input_opt:(Some "b.json") ~schema:true
     with
    | Error (Jx.Cli_args.Duplicate_input ("a.json", "b.json")) -> true
    | _ -> false)

let test_cli_args_resolve_query_stays_query () =
  Alcotest.(check string) "positional token stays query"
    "./users.json"
    (match
       Jx.Cli_args.resolve ~query:(Some "./users.json") ~input_file:None
         ~input_opt:None ~schema:true
     with
    | Ok resolved -> resolved.query
    | Error _ -> "")

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

let test_get_nested_path () =
  Alcotest.(check string)
    "get reads nested path"
    "\"Kyiv\""
    (eval_str "get .user.address.city"
       {|{"user":{"address":{"city":"Kyiv"}}}|})

let test_get_indexed_path () =
  Alcotest.(check string)
    "get reads indexed path"
    "\"Ada\""
    (eval_str "get .users[0].name"
       {|{"users":[{"name":"Ada"},{"name":"Linus"}]}|})

let test_get_stays_exact () =
  Alcotest.(check bool) "get does not auto-traverse arrays" true
    (match
       eval_error_kind "get .orders.items.name"
         {|{"orders":[{"items":[{"name":"W"}]}]}|}
     with
    | Some Jx.Error.Type_mismatch -> true
    | _ -> false)

let test_has_nested_true () =
  Alcotest.(check string)
    "has reports existing nested path"
    "true"
    (eval_str "has .config.features.dark_mode"
       {|{"config":{"features":{"dark_mode":true}}}|})

let test_has_nested_false () =
  Alcotest.(check string)
    "has reports missing nested path"
    "false"
    (eval_str "has .config.features.dark_mode" {|{"config":{"features":{}}}|})

let test_has_index_false () =
  Alcotest.(check string)
    "has reports missing index"
    "false"
    (eval_str "has .users[2].name" {|{"users":[{"name":"Ada"}]}|})

let test_pluck_simple () =
  Alcotest.(check string)
    "pluck traverses one collection step"
    {|["Ada","Linus"]|}
    (eval_str "pluck .users .name"
       {|{"users":[{"name":"Ada"},{"name":"Linus"}]}|})

let test_pluck_nested () =
  Alcotest.(check string)
    "pluck traverses nested collections"
    {|["W","G","D"]|}
    (eval_str "pluck .orders .items .name"
       {|{"orders":[{"items":[{"name":"W"},{"name":"G"}]},{"items":[{"name":"D"}]}]}|})

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

(* === Schema inference tests === *)

let schema_of input =
  let s = Jx.Schema.infer input in
  Jx.Printer.to_json ~compact:true (Jx.Schema.to_value s)

let schema_str json_str =
  schema_of (Jx.Simdjson_native.parse_value json_str)

let schema_query_str query json_str =
  let input = Jx.Simdjson_native.parse_value json_str in
  let ast = Jx.Parser.parse query in
  let result = Jx.Interpreter.eval [] input ast in
  schema_of result

let schema_sampled_query_str ~n query json_str =
  let input = Jx.Simdjson_native.parse_value json_str in
  let ast = Jx.Parser.parse query in
  let result = Jx.Interpreter.eval [] input ast in
  let s = Jx.Schema.infer_sampled ~n result in
  Jx.Printer.to_json ~compact:true (Jx.Schema.to_value s)

let schema_sampled_str ~n json_str =
  let input = Jx.Simdjson_native.parse_value json_str in
  let s = Jx.Schema.infer_sampled ~n input in
  Jx.Printer.to_json ~compact:true (Jx.Schema.to_value s)

let contains_substring s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let found = ref false in
    for i = 0 to slen - sublen do
      if (not !found) && String.sub s i sublen = sub then found := true
    done;
    !found

let is_map_schema result =
  contains_substring result "additionalProperties"

let test_schema_null () =
  Alcotest.(check string) "null schema"
    {|{"type":"null"}|}
    (schema_str "null")

let test_schema_boolean () =
  Alcotest.(check string) "boolean const"
    {|{"const":true}|}
    (schema_str "true")

let test_schema_integer () =
  Alcotest.(check string) "integer const"
    {|{"const":42}|}
    (schema_str "42")

let test_schema_number () =
  Alcotest.(check string) "number const"
    {|{"const":3.14}|}
    (schema_str "3.14")

let test_schema_string () =
  Alcotest.(check string) "string const"
    {|{"const":"hello"}|}
    (schema_str {|"hello"|})

let test_schema_simple_object () =
  Alcotest.(check string) "simple object schema"
    {|{"type":"object","properties":{"name":{"const":"Alice"},"age":{"const":30}},"required":["name","age"]}|}
    (schema_str {|{"name":"Alice","age":30}|})

let test_schema_nested_object () =
  Alcotest.(check string) "nested object schema"
    {|{"type":"object","properties":{"user":{"type":"object","properties":{"address":{"type":"object","properties":{"city":{"const":"NYC"}},"required":["city"]}},"required":["address"]}},"required":["user"]}|}
    (schema_str {|{"user":{"address":{"city":"NYC"}}}|})

let test_schema_homogeneous_array () =
  Alcotest.(check string) "homogeneous array with enum"
    {|{"type":"array","items":{"type":"integer","enum":[1,2,3]}}|}
    (schema_str "[1,2,3]")

let test_schema_array_of_objects () =
  Alcotest.(check string) "array of objects with enum"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]},"age":{"type":"integer","enum":[30,25]}},"required":["name","age"]}}|}
    (schema_str {|[{"name":"A","age":30},{"name":"B","age":25}]|})

let test_schema_nullable_field () =
  Alcotest.(check string) "nullable field with enum"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]},"age":{"type":["integer","null"],"enum":[30,null]}},"required":["name","age"]}}|}
    (schema_str {|[{"name":"A","age":30},{"name":"B","age":null}]|})

let test_schema_missing_field () =
  Alcotest.(check string) "missing field not in required"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]},"email":{"const":"a@b"}},"required":["name"]}}|}
    (schema_str {|[{"name":"A","email":"a@b"},{"name":"B"}]|})

let test_schema_heterogeneous_array () =
  Alcotest.(check string) "heterogeneous array"
    {|{"type":"array","items":{"oneOf":[{"type":"integer"},{"type":"string"},{"type":"null"}]}}|}
    (schema_str {|[1,"two",null]|})

let test_schema_empty_array () =
  Alcotest.(check string) "empty array"
    {|{"type":"array"}|}
    (schema_str "[]")

let test_schema_empty_object () =
  Alcotest.(check string) "empty object"
    {|{"type":"object","properties":{}}|}
    (schema_str "{}")

let test_schema_array_of_arrays () =
  Alcotest.(check string) "array of arrays"
    {|{"type":"array","items":{"type":"array","items":{"type":"integer","enum":[1,2,3,4]}}}|}
    (schema_str "[[1,2],[3,4]]")

let test_schema_int_float_widening () =
  Alcotest.(check string) "int/float widens to number"
    {|{"type":"array","items":{"type":"object","properties":{"val":{"type":"number","enum":[1,2.5,3]}},"required":["val"]}}|}
    (schema_str {|[{"val":1},{"val":2.5},{"val":3}]|})

let test_schema_nested_merge () =
  Alcotest.(check string) "nested object merge"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]},"addr":{"type":"object","properties":{"city":{"type":"string","enum":["NYC","LA"]},"zip":{"const":"10001"}},"required":["city"]}},"required":["name","addr"]}}|}
    (schema_str
       {|[{"name":"A","addr":{"city":"NYC","zip":"10001"}},{"name":"B","addr":{"city":"LA"}}]|})

let test_schema_with_query () =
  Alcotest.(check string) "schema after query"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]}},"required":["name"]}}|}
    (schema_query_str "map {name}"
       {|[{"name":"A","age":30},{"name":"B","age":25}]|})

let test_schema_sampled () =
  Alcotest.(check string) "sampled schema only sees first N"
    {|{"type":"array","items":{"type":"integer","enum":[1,2]}}|}
    (schema_sampled_str ~n:2 {|[1,2,"three","four"]|})

let test_schema_sampled_infinite_xd () =
  Alcotest.(check string) "sampled schema stops on infinite transducer output"
    {|{"type":"array","items":{"type":"integer","enum":[0,1,2]}}|}
    (schema_sampled_query_str ~n:3 "range | map ." "null")

let test_schema_bigint () =
  Alcotest.(check string) "bigint const"
    {|{"const":9223372036854775808}|}
    (schema_str "9223372036854775808")

let test_schema_enum_detection () =
  Alcotest.(check string) "enum detection on repeated values"
    {|{"type":"array","items":{"type":"object","properties":{"status":{"type":"string","enum":["active","inactive","pending"]},"level":{"type":"integer","enum":[1,2,3]}},"required":["status","level"]}}|}
    (schema_str
       {|[{"status":"active","level":1},{"status":"inactive","level":2},{"status":"active","level":3},{"status":"pending","level":1}]|})

let test_schema_const_detection () =
  Alcotest.(check string) "const for single-value field"
    {|{"type":"array","items":{"type":"object","properties":{"v":{"const":"same"}},"required":["v"]}}|}
    (schema_str {|[{"v":"same"},{"v":"same"},{"v":"same"}]|})

let test_schema_bool_collapse () =
  Alcotest.(check string) "both booleans collapse to type"
    {|{"type":"array","items":{"type":"object","properties":{"v":{"type":"boolean"}},"required":["v"]}}|}
    (schema_str {|[{"v":true},{"v":false},{"v":true}]|})

let test_schema_nullable_enum () =
  Alcotest.(check string) "nullable enum preserves values"
    {|{"type":"array","items":{"type":"object","properties":{"v":{"type":["string","null"],"enum":["a","b",null]}},"required":["v"]}}|}
    (schema_str {|[{"v":"a"},{"v":null},{"v":"b"}]|})

let test_schema_enum_threshold () =
  let many_values =
    let items =
      List.init 25 (fun i -> Printf.sprintf {|{"v":"val_%d"}|} i)
    in
    "[" ^ String.concat "," items ^ "]"
  in
  Alcotest.(check string) "enum collapses above threshold"
    {|{"type":"array","items":{"type":"object","properties":{"v":{"type":"string"}},"required":["v"]}}|}
    (schema_str many_values)

let test_schema_nullable_shorthand () =
  let items =
    List.init 25 (fun i -> Printf.sprintf {|{"v":"val_%d"}|} i)
  in
  let with_nulls = items @ [ {|{"v":null}|} ] in
  let json = "[" ^ String.concat "," with_nulls ^ "]" in
  Alcotest.(check string) "nullable uses type array shorthand"
    {|{"type":"array","items":{"type":"object","properties":{"v":{"type":["string","null"]}},"required":["v"]}}|}
    (schema_str json)

let test_schema_map_numeric_keys () =
  Alcotest.(check string) "numeric keys detected as map"
    {|{"type":"object","additionalProperties":{"type":"string","enum":["a","b","c"]}}|}
    (schema_str {|{"0":"a","1":"b","2":"c"}|})

let test_schema_map_numeric_single_key () =
  Alcotest.(check string) "single numeric key is map"
    {|{"type":"object","additionalProperties":{"const":"value"}}|}
    (schema_str {|{"0":"value"}|})

let test_schema_map_numeric_mixed_values () =
  Alcotest.(check string) "numeric keys with mixed value types"
    {|{"type":"object","additionalProperties":{"oneOf":[{"type":"integer"},{"type":"string"}]}}|}
    (schema_str {|{"0":1,"1":"hello"}|})

let test_schema_map_numeric_nullable_values () =
  Alcotest.(check string) "numeric keys with nullable values"
    {|{"type":"object","additionalProperties":{"type":["string","null"],"enum":["hello","world",null]}}|}
    (schema_str {|{"0":"hello","1":null,"2":"world"}|})

let test_schema_map_numeric_object_values () =
  Alcotest.(check string) "numeric keys with object values"
    {|{"type":"object","additionalProperties":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]}},"required":["name"]}}|}
    (schema_str {|{"0":{"name":"A"},"1":{"name":"B"}}|})

let test_schema_map_numeric_non_contiguous () =
  Alcotest.(check string) "non-contiguous numeric keys"
    {|{"type":"object","additionalProperties":{"type":"string","enum":["a","b","c"]}}|}
    (schema_str {|{"42":"a","100":"b","7":"c"}|})

let test_schema_map_dynamic_keys_uniform () =
  Alcotest.(check string) "array-merged disjoint keys detected as map"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":"string","enum":["Hello","Bonjour","Hallo"]}}}|}
    (schema_str {|[{"en":"Hello"},{"fr":"Bonjour"},{"de":"Hallo"}]|})

let test_schema_map_shared_keys_stays_record () =
  Alcotest.(check string) "array-merged with shared keys stays record"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"type":"string","enum":["A","B"]},"age":{"type":"integer","enum":[30,25]}},"required":["name","age"]}}|}
    (schema_str {|[{"name":"A","age":30},{"name":"B","age":25}]|})

let test_schema_map_dynamic_keys_nonuniform () =
  Alcotest.(check bool) "disjoint keys with mixed types stays record"
    false
    (is_map_schema
       (schema_str {|[{"a":1},{"b":"hello"},{"c":true}]|}))

let test_schema_map_single_object_no_detect () =
  Alcotest.(check bool) "single object with many string keys stays record"
    false
    (let items = List.init 25 (fun i -> Printf.sprintf {|"key_%d":"val_%d"|} i i) in
     let json = "{" ^ String.concat "," items ^ "}" in
     is_map_schema (schema_str json))

let test_schema_map_partial_overlap_stays_record () =
  Alcotest.(check string) "partial key overlap stays record"
    {|{"type":"array","items":{"type":"object","properties":{"id":{"type":"integer","enum":[1,2]},"name":{"const":"A"},"email":{"const":"a@b"}},"required":["id"]}}|}
    (schema_str {|[{"id":1,"name":"A"},{"id":2,"email":"a@b"}]|})

let test_schema_map_not_all_numeric () =
  Alcotest.(check string) "mixed numeric/non-numeric stays record"
    {|{"type":"object","properties":{"0":{"const":"a"},"1":{"const":"b"},"name":{"const":"c"}},"required":["0","1","name"]}|}
    (schema_str {|{"0":"a","1":"b","name":"c"}|})

let test_schema_map_array_of_maps () =
  Alcotest.(check string) "array of map objects"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":"string","enum":["a","c","b","d"]}}}|}
    (schema_str {|[{"0":"a","1":"b"},{"0":"c","2":"d"}]|})

let test_schema_map_nested_in_record () =
  Alcotest.(check string) "map nested inside record"
    {|{"type":"object","properties":{"name":{"const":"test"},"data":{"type":"object","additionalProperties":{"type":"string","enum":["a","b","c"]}}},"required":["name","data"]}|}
    (schema_str {|{"name":"test","data":{"0":"a","1":"b","2":"c"}}|})

let test_schema_map_with_query () =
  Alcotest.(check string) "map detected after query"
    {|{"type":"object","additionalProperties":{"type":"string","enum":["a","b","c"]}}|}
    (schema_query_str ".data"
       {|{"data":{"0":"a","1":"b","2":"c"}}|})

let test_schema_map_single_key_objects () =
  Alcotest.(check string) "array of single-key objects as map"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":"object","properties":{"score":{"type":"integer","enum":[95,87]}},"required":["score"]}}}|}
    (schema_str
       {|[{"user_1":{"score":95}},{"user_2":{"score":87}}]|})

let test_schema_map_boolean_flags () =
  Alcotest.(check string) "boolean feature flags as map"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":"boolean"}}}|}
    (schema_str
       {|[{"feat_a":true},{"feat_b":false},{"feat_c":true}]|})

let test_schema_map_one_shared_key_saves () =
  Alcotest.(check string) "one shared key prevents map"
    {|{"type":"array","items":{"type":"object","properties":{"id":{"type":"integer","enum":[1,2,3]},"a":{"const":"x"},"b":{"const":"y"},"c":{"const":"z"}},"required":["id"]}}|}
    (schema_str
       {|[{"id":1,"a":"x"},{"id":2,"b":"y"},{"id":3,"c":"z"}]|})

let test_schema_map_identical_keys () =
  Alcotest.(check string) "identical keys across objects stays record"
    {|{"type":"array","items":{"type":"object","properties":{"a":{"type":"integer","enum":[1,3]},"b":{"type":"integer","enum":[2,4]}},"required":["a","b"]}}|}
    (schema_str {|[{"a":1,"b":2},{"a":3,"b":4}]|})

let test_schema_map_chain_overlap () =
  Alcotest.(check string) "chain overlap stays record"
    {|{"type":"array","items":{"type":"object","properties":{"a":{"const":1},"b":{"type":"integer","enum":[2,3]},"c":{"type":"integer","enum":[4,5]},"d":{"const":6}}}}|}
    (schema_str
       {|[{"a":1,"b":2},{"b":3,"c":4},{"c":5,"d":6}]|})

let test_schema_map_empty_object_no_poison () =
  Alcotest.(check string) "empty object does not trigger map"
    {|{"type":"array","items":{"type":"object","properties":{"name":{"const":"A"},"city":{"const":"NYC"}}}}|}
    (schema_str {|[{"name":"A","city":"NYC"},{}]|})

let test_schema_map_two_different_records () =
  Alcotest.(check string) "two different record types stays record"
    {|{"type":"array","items":{"type":"object","properties":{"type":{"const":"user"},"name":{"const":"A"},"status":{"const":"ok"},"message":{"const":"done"}}}}|}
    (schema_str
       {|[{"type":"user","name":"A"},{"status":"ok","message":"done"}]|})

let test_schema_map_enum_collapse_in_values () =
  let items =
    List.init 25 (fun i ->
      Printf.sprintf {|{"%d":"val_%d"}|} i i)
  in
  let json = "[" ^ String.concat "," items ^ "]" in
  Alcotest.(check string) "map values collapse past enum threshold"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":"string"}}}|}
    (schema_str json)

let test_schema_map_nested_map_in_map () =
  Alcotest.(check string) "nested map detection"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":"object","additionalProperties":{"type":"integer","enum":[1,3,2,4]}}}}|}
    (schema_str
       {|[{"x":{"0":1,"1":2}},{"y":{"0":3,"1":4}}]|})

let test_schema_map_nullable_values_in_array () =
  Alcotest.(check string) "map with nullable values in array context"
    {|{"type":"array","items":{"type":"object","additionalProperties":{"type":["string","null"],"enum":["x","y",null]}}}|}
    (schema_str {|[{"a":"x"},{"b":null},{"c":"y"}]|})

let test_schema_oneof_base_subsumes_enum () =
  let json = Printf.sprintf {|[%s]|}
    (String.concat ","
      (List.init 25 (fun i -> Printf.sprintf {|{"code":"s%d"}|} i)
       @ [ {|{"code":42}|}; {|{"code":"specific"}|} ]))
  in
  let result = schema_str json in
  let has_string_enum = contains_substring result {|"string","enum"|} in
  Alcotest.(check bool) "no string enum when bare string exists" false has_string_enum

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
      ( "cli",
        [
          Alcotest.test_case "missing query" `Quick test_cli_args_resolve_missing_query;
          Alcotest.test_case "schema file flag" `Quick test_cli_args_resolve_schema_file_flag;
          Alcotest.test_case "duplicate input" `Quick test_cli_args_resolve_duplicate_input;
          Alcotest.test_case "query stays query" `Quick test_cli_args_resolve_query_stays_query;
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
          Alcotest.test_case "get nested path" `Quick test_get_nested_path;
          Alcotest.test_case "get indexed path" `Quick test_get_indexed_path;
          Alcotest.test_case "get stays exact" `Quick test_get_stays_exact;
          Alcotest.test_case "has nested true" `Quick test_has_nested_true;
          Alcotest.test_case "has nested false" `Quick test_has_nested_false;
          Alcotest.test_case "has index false" `Quick test_has_index_false;
          Alcotest.test_case "pluck simple" `Quick test_pluck_simple;
          Alcotest.test_case "pluck nested" `Quick test_pluck_nested;
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
      ( "schema",
        [
          Alcotest.test_case "null" `Quick test_schema_null;
          Alcotest.test_case "boolean" `Quick test_schema_boolean;
          Alcotest.test_case "integer" `Quick test_schema_integer;
          Alcotest.test_case "number" `Quick test_schema_number;
          Alcotest.test_case "string" `Quick test_schema_string;
          Alcotest.test_case "simple object" `Quick test_schema_simple_object;
          Alcotest.test_case "nested object" `Quick test_schema_nested_object;
          Alcotest.test_case "homogeneous array" `Quick test_schema_homogeneous_array;
          Alcotest.test_case "array of objects" `Quick test_schema_array_of_objects;
          Alcotest.test_case "nullable field" `Quick test_schema_nullable_field;
          Alcotest.test_case "missing field" `Quick test_schema_missing_field;
          Alcotest.test_case "heterogeneous array" `Quick test_schema_heterogeneous_array;
          Alcotest.test_case "empty array" `Quick test_schema_empty_array;
          Alcotest.test_case "empty object" `Quick test_schema_empty_object;
          Alcotest.test_case "array of arrays" `Quick test_schema_array_of_arrays;
          Alcotest.test_case "int/float widening" `Quick test_schema_int_float_widening;
          Alcotest.test_case "nested merge" `Quick test_schema_nested_merge;
          Alcotest.test_case "with query" `Quick test_schema_with_query;
          Alcotest.test_case "sampled" `Quick test_schema_sampled;
          Alcotest.test_case "sampled infinite xd" `Quick test_schema_sampled_infinite_xd;
          Alcotest.test_case "bigint" `Quick test_schema_bigint;
          Alcotest.test_case "enum detection" `Quick test_schema_enum_detection;
          Alcotest.test_case "const detection" `Quick test_schema_const_detection;
          Alcotest.test_case "bool collapse" `Quick test_schema_bool_collapse;
          Alcotest.test_case "nullable enum" `Quick test_schema_nullable_enum;
          Alcotest.test_case "enum threshold" `Quick test_schema_enum_threshold;
          Alcotest.test_case "nullable shorthand" `Quick test_schema_nullable_shorthand;
        ] );
      ( "schema-maps",
        [
          Alcotest.test_case "numeric keys" `Quick test_schema_map_numeric_keys;
          Alcotest.test_case "numeric single key" `Quick test_schema_map_numeric_single_key;
          Alcotest.test_case "numeric mixed values" `Quick test_schema_map_numeric_mixed_values;
          Alcotest.test_case "numeric nullable values" `Quick test_schema_map_numeric_nullable_values;
          Alcotest.test_case "numeric object values" `Quick test_schema_map_numeric_object_values;
          Alcotest.test_case "numeric non-contiguous" `Quick test_schema_map_numeric_non_contiguous;
          Alcotest.test_case "dynamic keys uniform" `Quick test_schema_map_dynamic_keys_uniform;
          Alcotest.test_case "shared keys stays record" `Quick test_schema_map_shared_keys_stays_record;
          Alcotest.test_case "dynamic keys nonuniform" `Quick test_schema_map_dynamic_keys_nonuniform;
          Alcotest.test_case "single object no detect" `Quick test_schema_map_single_object_no_detect;
          Alcotest.test_case "partial overlap stays record" `Quick test_schema_map_partial_overlap_stays_record;
          Alcotest.test_case "not all numeric" `Quick test_schema_map_not_all_numeric;
          Alcotest.test_case "array of maps" `Quick test_schema_map_array_of_maps;
          Alcotest.test_case "nested in record" `Quick test_schema_map_nested_in_record;
          Alcotest.test_case "map with query" `Quick test_schema_map_with_query;
          Alcotest.test_case "single-key objects" `Quick test_schema_map_single_key_objects;
          Alcotest.test_case "boolean flags" `Quick test_schema_map_boolean_flags;
          Alcotest.test_case "one shared key saves" `Quick test_schema_map_one_shared_key_saves;
          Alcotest.test_case "identical keys" `Quick test_schema_map_identical_keys;
          Alcotest.test_case "chain overlap" `Quick test_schema_map_chain_overlap;
          Alcotest.test_case "empty object no poison" `Quick test_schema_map_empty_object_no_poison;
          Alcotest.test_case "two different records" `Quick test_schema_map_two_different_records;
          Alcotest.test_case "enum collapse in values" `Quick test_schema_map_enum_collapse_in_values;
          Alcotest.test_case "nested map in map" `Quick test_schema_map_nested_map_in_map;
          Alcotest.test_case "nullable values in array" `Quick test_schema_map_nullable_values_in_array;
          Alcotest.test_case "oneof base subsumes enum" `Quick test_schema_oneof_base_subsumes_enum;
        ] );
    ]
