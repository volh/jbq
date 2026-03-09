let () = Jx.Interpreter.dispatch_ref := Jx.Stdlib_fns.dispatch

let eval_str query json_str =
  let json = Yojson.Basic.from_string json_str in
  let input = Jx.Value.of_yojson json in
  let ast = Jx.Parser.parse query in
  let result = Jx.Interpreter.eval [] input ast in
  Jx.Printer.to_json ~compact:true result

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

let test_optional_field () =
  Alcotest.(check string)
    "optional field" "null"
    (eval_str ".missing?" {|{"name": "Alice"}|})

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
      ( "core",
        [
          Alcotest.test_case "identity" `Quick test_identity;
          Alcotest.test_case "field access" `Quick test_field_access;
          Alcotest.test_case "nested field" `Quick test_nested_field;
          Alcotest.test_case "pipe" `Quick test_pipe;
          Alcotest.test_case "arithmetic" `Quick test_arithmetic;
          Alcotest.test_case "string concat" `Quick test_string_concat;
          Alcotest.test_case "null coalesce" `Quick test_null_coalesce;
          Alcotest.test_case "conditional" `Quick test_conditional;
          Alcotest.test_case "let binding" `Quick test_let_binding;
          Alcotest.test_case "array index" `Quick test_array_index;
          Alcotest.test_case "negative index" `Quick test_negative_index;
          Alcotest.test_case "slice" `Quick test_slice;
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
          Alcotest.test_case "unique" `Quick test_unique;
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
