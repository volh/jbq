let () =
  Jx.Interpreter.dispatch_ref := Jx.Stdlib_fns.dispatch;
  Jx.Value.xd_run_ref := Jx.Transducer.run

let run query json_str =
  let json = Yojson.Basic.from_string json_str in
  let input = Jx.Value.of_yojson json in
  let ast = Jx.Parser.parse query in
  Jx.Interpreter.eval [] input ast

let run_null query = run query "null"

let run_preparse query input =
  let ast = Jx.Parser.parse query in
  Jx.Interpreter.eval [] input ast

let preparse json_str =
  let json = Yojson.Basic.from_string json_str in
  Jx.Value.of_yojson json

let time_it label f =
  Gc.compact ();
  let stat_before = Gc.stat () in
  let t0 = Sys.time () in
  let result = f () in
  let t1 = Sys.time () in
  let stat_after = Gc.stat () in
  let alloc_words =
    stat_after.minor_words +. stat_after.major_words
    -. stat_before.minor_words -. stat_before.major_words
  in
  let alloc_kb = alloc_words *. 8.0 /. 1024.0 in
  Printf.printf "  %-50s %8.3f ms  %10.0f KB allocated\n%!"
    label
    ((t1 -. t0) *. 1000.0)
    alloc_kb;
  result

let big_array n =
  let buf = Buffer.create (n * 4) in
  Buffer.add_char buf '[';
  for i = 0 to n - 1 do
    if i > 0 then Buffer.add_char buf ',';
    Buffer.add_string buf (string_of_int i)
  done;
  Buffer.add_char buf ']';
  Buffer.contents buf

let () =
  Printf.printf "\n";
  Printf.printf "==========================================================\n";
  Printf.printf "  jx Lazy Sequence Benchmark\n";
  Printf.printf "==========================================================\n";

  (* --- Test 1: Proof that infinite sequences work --- *)
  Printf.printf "\n--- Proof: infinite sequences terminate ---\n\n";

  ignore (time_it "range | take 5"
    (fun () -> run_null "range | take 5"));

  ignore (time_it "range | where (. > 1000000) | take 1"
    (fun () -> run_null "range | where (. > 1000000) | take 1"));

  ignore (time_it "range | map (. * .) | where (. > 1000000) | first"
    (fun () -> run_null "range | map (. * .) | where (. > 1000000) | first"));

  ignore (time_it "range | where (. % 7 == 0) | skip 1000 | take 3"
    (fun () -> run_null "range | where (. % 7 == 0) | skip 1000 | take 3"));

  (* --- Test 2: Lazy vs eager allocation --- *)
  Printf.printf "\n--- Lazy vs eager: take 10 from different sizes ---\n\n";

  List.iter
    (fun n ->
      let label = Printf.sprintf "eager %dK array | take 10" (n / 1000) in
      let big = big_array n in
      ignore (time_it label (fun () -> run "take 10" big)))
    [ 10_000; 100_000; 1_000_000 ];

  ignore (time_it "lazy  infinite   | take 10"
    (fun () -> run_null "range | take 10"));

  (* --- Test 3: Lazy vs eager filter --- *)
  Printf.printf "\n--- Lazy vs eager: filter + take 5 ---\n\n";

  List.iter
    (fun n ->
      let label = Printf.sprintf "eager %dK array | where (. > n-10) | take 5" (n / 1000) in
      let big = big_array n in
      let query = Printf.sprintf "where (. > %d) | take 5" (n - 10) in
      ignore (time_it label (fun () -> run query big)))
    [ 10_000; 100_000; 1_000_000 ];

  ignore (time_it "lazy  infinite   | where (. > 999990) | take 5"
    (fun () -> run_null "range | where (. > 999990) | take 5"));

  (* --- Test 4: Lazy aggregation --- *)
  Printf.printf "\n--- Lazy aggregation: sum of first N ---\n\n";

  ignore (time_it "lazy  range | take 100     | sum"
    (fun () -> run_null "range | take 100 | sum"));

  ignore (time_it "lazy  range | take 10000   | sum"
    (fun () -> run_null "range | take 10000 | sum"));

  ignore (time_it "lazy  range | take 1000000 | sum"
    (fun () -> run_null "range | take 1000000 | sum"));

  Printf.printf "\n  Note: lazy sum allocates O(1) — only the accumulator.\n";
  Printf.printf "  The sequence is never materialized as an array.\n";

  (* --- Test 5: Complex lazy pipeline --- *)
  Printf.printf "\n--- Complex pipeline: map + where + skip + take ---\n\n";

  ignore (time_it "lazy  range | map(.*.) | where(>1M) | skip 100 | take 5"
    (fun () -> run_null "range | map (. * .) | where (. > 1000000) | skip 100 | take 5"));

  ignore (time_it "lazy  range | map(.*.) | where(%3==0) | take 10 | avg"
    (fun () -> run_null "range | map (. * .) | where (. % 3 == 0) | take 10 | avg"));

  (* --- Test 6: Pipeline-only cost (pre-parsed input) --- *)
  Printf.printf "\n--- Pipeline-only (input pre-parsed, no JSON overhead) ---\n\n";

  let pre_10k = preparse (big_array 10_000) in
  let pre_100k = preparse (big_array 100_000) in
  let pre_1m = preparse (big_array 1_000_000) in

  ignore (time_it "10K  pre-parsed | where (. > n-10) | take 5"
    (fun () -> run_preparse (Printf.sprintf "where (. > %d) | take 5" (10_000 - 10)) pre_10k));

  ignore (time_it "100K pre-parsed | where (. > n-10) | take 5"
    (fun () -> run_preparse (Printf.sprintf "where (. > %d) | take 5" (100_000 - 10)) pre_100k));

  ignore (time_it "1M   pre-parsed | where (. > n-10) | take 5"
    (fun () -> run_preparse (Printf.sprintf "where (. > %d) | take 5" (1_000_000 - 10)) pre_1m));

  Printf.printf "\n";

  ignore (time_it "10K  pre-parsed | map (. * 2) | take 5"
    (fun () -> run_preparse "map (. * 2) | take 5" pre_10k));

  ignore (time_it "100K pre-parsed | map (. * 2) | take 5"
    (fun () -> run_preparse "map (. * 2) | take 5" pre_100k));

  ignore (time_it "1M   pre-parsed | map (. * 2) | take 5"
    (fun () -> run_preparse "map (. * 2) | take 5" pre_1m));

  Printf.printf "\n";

  ignore (time_it "10K  pre-parsed | where | map | take 3"
    (fun () -> run_preparse "where (. > 5000) | map (. * .) | take 3" pre_10k));

  ignore (time_it "100K pre-parsed | where | map | take 3"
    (fun () -> run_preparse "where (. > 50000) | map (. * .) | take 3" pre_100k));

  ignore (time_it "1M   pre-parsed | where | map | take 3"
    (fun () -> run_preparse "where (. > 500000) | map (. * .) | take 3" pre_1m));

  Printf.printf "\n==========================================================\n";
  Printf.printf "  Done.\n";
  Printf.printf "==========================================================\n"
