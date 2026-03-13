type kind =
  | Parse_error
  | Key_not_found
  | Index_out_of_bounds
  | Type_mismatch
  | Null_access
  | Unknown_function
  | Arity_mismatch
  | Runtime_error

type t = {
  kind : kind;
  message : string;
  loc : Ast.loc option;
  suggestion : string option;
}

exception Jbq_error of t

let make ?loc ?suggestion kind message = { kind; message; loc; suggestion }

let raise_ ?loc ?suggestion kind message =
  raise (Jbq_error (make ?loc ?suggestion kind message))

let kind_to_string = function
  | Parse_error -> "parse_error"
  | Key_not_found -> "key_not_found"
  | Index_out_of_bounds -> "index_out_of_bounds"
  | Type_mismatch -> "type_mismatch"
  | Null_access -> "null_access"
  | Unknown_function -> "unknown_function"
  | Arity_mismatch -> "arity_mismatch"
  | Runtime_error -> "runtime_error"

let format_error (err : t) (source : string) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "error[%s]: %s\n" (kind_to_string err.kind) err.message);
  (match err.loc with
  | Some loc when String.length source > 0 ->
    Buffer.add_string buf "  |\n";
    Buffer.add_string buf (Printf.sprintf "  | %s\n" source);
    Buffer.add_string buf "  | ";
    for _ = 1 to loc.start_pos do
      Buffer.add_char buf ' '
    done;
    let len = max 1 (loc.end_pos - loc.start_pos) in
    for _ = 1 to len do
      Buffer.add_char buf '^'
    done;
    Buffer.add_char buf '\n'
  | _ -> ());
  (match err.suggestion with
  | Some s -> Buffer.add_string buf (Printf.sprintf "  = help: %s\n" s)
  | None -> ());
  Buffer.contents buf
