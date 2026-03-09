let () =
  Jx.Interpreter.dispatch_ref := Jx.Stdlib_fns.dispatch;
  Jx.Value.xd_run_ref := Jx.Transducer.run

let profile = match Sys.getenv_opt "JX_PROFILE" with Some "1" -> true | _ -> false

let time label f =
  if profile then (
    let t0 = Unix.gettimeofday () in
    let result = f () in
    let dt = Unix.gettimeofday () -. t0 in
    Printf.eprintf "  %-25s %8.3f ms\n%!" label (dt *. 1000.0);
    result)
  else f ()

let streamable_root_fn = function
  | "where" | "map" | "unique" | "flatten" | "take" | "skip"
  | "sort_by" | "group_by" | "reverse" | "first" | "last"
  | "count" | "length" | "sum" | "min" | "max" | "avg" ->
    true
  | _ -> false

let rec supports_simdjson_top_array_path (expr : Jx.Ast.expr) =
  match expr with
  | Identity -> true
  | FnCall { name; _ } -> streamable_root_fn name
  | Pipe { left; right } ->
    supports_simdjson_top_array_path left
    && supports_simdjson_top_array_path right
  | _ -> false

let run query_str input_source raw_output compact =
  try
    let ast = time "parse query" (fun () -> Jx.Parser.parse query_str) in
    let json_str =
      time "read file" (fun () ->
        match input_source with
        | Some path ->
          let ic = open_in path in
          let n = in_channel_length ic in
          let s = Bytes.create n in
          really_input ic s 0 n;
          close_in ic;
          Bytes.to_string s
        | None ->
          let buf = Buffer.create 4096 in
          (try
             while true do
               Buffer.add_char buf (input_char stdin)
             done
           with End_of_file -> ());
          Buffer.contents buf)
    in
    let input =
      if supports_simdjson_top_array_path ast then
        try
          time "simdjson top-array init" (fun () ->
            Jx.Simdjson_stream.top_array_input json_str)
        with Failure _ ->
          time "simdjson parse" (fun () -> Jx.Simdjson_native.parse_value json_str)
      else
        time "simdjson parse" (fun () -> Jx.Simdjson_native.parse_value json_str)
    in
    let result = time "eval pipeline" (fun () -> Jx.Interpreter.eval [] input ast) in
    let output =
      time "output" (fun () ->
        match (raw_output, result) with
        | true, Jx.Value.String s -> s
        | _ -> Jx.Printer.to_json ~compact result)
    in
    time "print" (fun () ->
      print_string output;
      print_newline ());
    0
  with
  | Jx.Error.Jx_error err ->
    Printf.eprintf "%s%!" (Jx.Error.format_error err query_str);
    1
  | Failure msg ->
    let prefix = "json: " in
    if String.length msg >= String.length prefix
       && String.sub msg 0 (String.length prefix) = prefix
    then
      Printf.eprintf "error[json]: %s\n"
        (String.sub msg (String.length prefix)
           (String.length msg - String.length prefix))
    else
      Printf.eprintf "error: %s\n" msg;
    1

open Cmdliner

let query =
  let doc = "The jx query expression." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let input_file =
  let doc = "Input JSON file. Reads from stdin if not provided." in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"FILE" ~doc)

let raw_output =
  let doc = "Output raw strings without quotes." in
  Arg.(value & flag & info [ "r"; "raw-output" ] ~doc)

let compact =
  let doc = "Compact output (no pretty-printing)." in
  Arg.(value & flag & info [ "c"; "compact" ] ~doc)

let cmd =
  let doc = "A better query language for JSON" in
  let info = Cmd.info "jx" ~version:"0.1.0" ~doc in
  Cmd.v info
    Term.(const run $ query $ input_file $ raw_output $ compact)

let () = exit (Cmd.eval' cmd)
