let () =
  Jx.Interpreter.dispatch_ref := Jx.Stdlib_fns.dispatch;
  Jx.Value.xd_run_ref := Jx.Transducer.run

let run query_str input_source raw_output compact =
  try
    let json_str =
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
        Buffer.contents buf
    in
    let json = Yojson.Basic.from_string json_str in
    let input = Jx.Value.of_yojson json in
    let ast = Jx.Parser.parse query_str in
    let result = Jx.Interpreter.eval [] input ast in
    let output =
      match (raw_output, result) with
      | true, Jx.Value.String s -> s
      | _ -> Jx.Printer.to_json ~compact result
    in
    print_string output;
    print_newline ();
    0
  with
  | Jx.Error.Jx_error err ->
    Printf.eprintf "%s%!" (Jx.Error.format_error err query_str);
    1
  | Yojson.Json_error msg ->
    Printf.eprintf "error[json]: %s\n" msg;
    1
  | Failure msg ->
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
