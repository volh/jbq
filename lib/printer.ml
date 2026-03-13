let write_string buf s =
  Buffer.add_char buf '"';
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\b' -> Buffer.add_string buf "\\b"
    | '\012' -> Buffer.add_string buf "\\f"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 0x20 ->
      Printf.bprintf buf "\\u%04x" (Char.code c)
    | c -> Buffer.add_char buf c
  done;
  Buffer.add_char buf '"'

let float_needs_period s =
  let rec loop i =
    if i >= String.length s then true
    else match String.unsafe_get s i with
      | '0'..'9' | '-' -> loop (i + 1)
      | _ -> false
  in
  loop 0

let raise_non_finite_float f =
  let kind =
    if classify_float f = FP_nan then "NaN"
    else if f > 0. then "Infinity"
    else "-Infinity"
  in
  Error.raise_ Runtime_error
    (Printf.sprintf "cannot render non-finite float %s as JSON" kind)

let write_float buf f =
  match classify_float f with
  | FP_nan | FP_infinite -> raise_non_finite_float f
  | _ ->
    let s16 = Printf.sprintf "%.16g" f in
    let s =
      if float_of_string s16 = f then s16
      else Printf.sprintf "%.17g" f
    in
    Buffer.add_string buf s;
    if float_needs_period s then Buffer.add_string buf ".0"

let write_bigint buf z =
  Buffer.add_string buf (Z.to_string z)

let compact_size (v : Value.t) : int =
  let n = ref 0 in
  let limit = 80 in
  let exception Too_wide in
  let add k = n := !n + k; if !n > limit then raise_notrace Too_wide in
  let rec walk : Value.t -> unit = function
    | Null -> add 4
    | Bool true -> add 4
    | Bool false -> add 5
    | Int i -> add (String.length (string_of_int i))
    | BigInt z -> add (String.length (Z.to_string z))
    | Float _ -> add 20
    | String s -> add (String.length s + 2)
    | Array [] -> add 2
    | Array xs ->
      add 2;
      List.iter (fun x -> walk x; add 2) xs
    | Object obj when Array.length obj.fields = 0 -> add 2
    | Object obj ->
      let kvs = obj.fields in
      add 2;
      Array.iter (fun (k, v) -> add (String.length k + 4); walk v; add 2) kvs
    | Seq s ->
      add 2;
      Seq.iter (fun x -> walk x; add 2) s
    | Xd (source, xd) ->
      walk (Value.Array (!Value.xd_run_ref xd (Value.to_seq_of source)))
  in
  (try walk v with Too_wide -> ());
  !n

let rec write_compact buf (v : Value.t) =
  match v with
  | Null -> Buffer.add_string buf "null"
  | Bool true -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | Int i -> Buffer.add_string buf (string_of_int i)
  | BigInt z -> write_bigint buf z
  | Float f -> write_float buf f
  | String s -> write_string buf s
  | Array [] -> Buffer.add_string buf "[]"
  | Array xs ->
    Buffer.add_char buf '[';
    List.iteri (fun i x ->
      if i > 0 then Buffer.add_char buf ',';
      write_compact buf x) xs;
    Buffer.add_char buf ']'
  | Object _ when Array.length (Value.object_entries v) = 0 -> Buffer.add_string buf "{}"
  | Object _ ->
    let kvs = Value.object_entries v in
    Buffer.add_char buf '{';
    Array.iteri (fun i (k, v) ->
      if i > 0 then Buffer.add_char buf ',';
      write_string buf k;
      Buffer.add_char buf ':';
      write_compact buf v) kvs;
    Buffer.add_char buf '}'
  | Seq s -> write_compact buf (Value.Array (List.of_seq s))
  | Xd (source, xd) ->
    write_compact buf (Value.Array (!Value.xd_run_ref xd (Value.to_seq_of source)))

let newline_indent buf depth =
  Buffer.add_char buf '\n';
  for _ = 1 to depth do
    Buffer.add_string buf "  "
  done

let rec write_pretty buf depth (v : Value.t) =
  match v with
  | Null | Bool _ | Int _ | BigInt _ | Float _ | String _ ->
    write_compact buf v
  | Array [] -> Buffer.add_string buf "[]"
  | Array xs ->
    if compact_size v <= 80 then (
      Buffer.add_string buf "[ ";
      List.iteri (fun i x ->
        if i > 0 then Buffer.add_string buf ", ";
        write_pretty buf depth x) xs;
      Buffer.add_string buf " ]")
    else (
      Buffer.add_char buf '[';
      List.iteri (fun i x ->
        if i > 0 then Buffer.add_char buf ',';
        newline_indent buf (depth + 1);
        write_pretty buf (depth + 1) x) xs;
      newline_indent buf depth;
      Buffer.add_char buf ']')
  | Object _ ->
    let kvs = Value.object_entries v in
    if Array.length kvs = 0 then Buffer.add_string buf "{}"
    else if compact_size v <= 80 then (
      Buffer.add_string buf "{ ";
      Array.iteri (fun i (k, v) ->
        if i > 0 then Buffer.add_string buf ", ";
        write_string buf k;
        Buffer.add_string buf ": ";
        write_pretty buf depth v) kvs;
      Buffer.add_string buf " }")
    else (
      Buffer.add_char buf '{';
      Array.iteri (fun i (k, v) ->
        if i > 0 then Buffer.add_char buf ',';
        newline_indent buf (depth + 1);
        write_string buf k;
        Buffer.add_string buf ": ";
        write_pretty buf (depth + 1) v) kvs;
      newline_indent buf depth;
      Buffer.add_char buf '}')
  | Seq s -> write_pretty buf depth (Value.Array (List.of_seq s))
  | Xd (source, xd) ->
    write_pretty buf depth (Value.Array (!Value.xd_run_ref xd (Value.to_seq_of source)))

let to_json ?(compact = false) (value : Value.t) : string =
  let v = Value.realize value in
  let buf = Buffer.create 4096 in
  if compact then write_compact buf v
  else write_pretty buf 0 v;
  Buffer.contents buf
