open Ast

let eval_in_ctx env input expr = Interpreter.eval env input expr

type unique_key =
  | KNull
  | KBool of bool
  | KInt of int
  | KBigInt of string
  | KFloat of int64
  | KString of string
  | KArray of unique_key list
  | KObject of (string * unique_key) array

let rec unique_key_of_value value =
  match Value.realize value with
  | Value.Null -> KNull
  | Value.Bool b -> KBool b
  | Value.Int i -> KInt i
  | Value.BigInt z -> KBigInt (Z.to_string z)
  | Value.Float f -> KFloat (Int64.bits_of_float f)
  | Value.String s -> KString s
  | Value.Array xs -> KArray (List.map unique_key_of_value xs)
  | Value.Object _ ->
    let kvs = Value.object_entries value in
    KObject (Array.map (fun (k, v) -> (k, unique_key_of_value v)) kvs)
  | Value.Seq _ | Value.Xd _ -> assert false

let require_collection name input loc =
  if not (Value.is_collection input) then
    Error.raise_ ~loc Type_mismatch
      (Printf.sprintf "%s requires array or sequence, got %s" name
         (Value.type_name input))

let as_seq input =
  match input with
  | Value.Array xs -> List.to_seq xs
  | Value.Seq s -> s
  | Value.Xd (source, xd) ->
    List.to_seq (!Value.xd_run_ref xd (Value.to_seq source))
  | _ -> Seq.return input

let fold_collection f init input =
  match input with
  | Value.Xd (source, xd) ->
    Transducer.fold xd f init (Value.to_seq source)
  | _ ->
    Seq.fold_left f init (as_seq input)

let xd_compose input new_xd =
  match input with
  | Value.Xd (source, existing) ->
    Value.Xd (source, Transducer.compose existing new_xd)
  | _ -> Value.Xd (input, new_xd)

let realize_input input =
  match input with
  | Value.Xd (source, xd) ->
    Value.Array (!Value.xd_run_ref xd (Value.to_seq source))
  | Value.Seq s -> Value.Array (List.of_seq s)
  | v -> v

let all_fn_names =
  [
    "where"; "map"; "flatmap"; "sort_by"; "group_by"; "unique"; "flatten"; "reverse";
    "first"; "last"; "take"; "skip"; "count"; "length";
    "sum"; "min"; "max"; "avg";
    "lower"; "upper"; "trim"; "truncate"; "split"; "join";
    "keys"; "values"; "pick"; "omit";
    "type"; "to_string"; "to_number";
    "range";
  ]

let raise_unknown name loc =
  let suggestion = Interpreter.find_closest name all_fn_names in
  Error.raise_ ~loc
    ?suggestion:
      (match suggestion with
      | Some s -> Some (Printf.sprintf "did you mean %s?" s)
      | None ->
        Some
          (Printf.sprintf "available functions: %s"
             (String.concat ", " all_fn_names)))
    Unknown_function
    (Printf.sprintf "unknown function: %s" name)

let eval_per_item env expr item =
  match expr with
  | Lambda { param; body; _ } -> eval_in_ctx ((param, item) :: env) item body
  | _ -> eval_in_ctx env item expr

let dispatch env input name args loc =
  match (name, args) with
  (* === Transducible collection functions === *)
  | "where", [ pred ] ->
    require_collection "where" input loc;
    let xd = Transducer.filter
      (fun item -> Value.is_truthy (eval_per_item env pred item)) in
    xd_compose input xd
  | "map", [ expr ] ->
    require_collection "map" input loc;
    let xd = Transducer.map (eval_per_item env expr) in
    xd_compose input xd
  | "unique", [] ->
    require_collection "unique" input loc;
    xd_compose input (Transducer.unique unique_key_of_value)
  | "flatmap", [ expr ] ->
    require_collection "flatmap" input loc;
    xd_compose input (Transducer.flatmap (eval_per_item env expr))
  | "flatten", [] ->
    require_collection "flatten" input loc;
    xd_compose input Transducer.flatten
  | "take", [ n_expr ] ->
    let n =
      match eval_in_ctx env input n_expr with
      | Value.Int n -> n
      | _ -> Error.raise_ ~loc Type_mismatch "take requires integer argument"
    in
    require_collection "take" input loc;
    xd_compose input (Transducer.take n)
  | "skip", [ n_expr ] ->
    let n =
      match eval_in_ctx env input n_expr with
      | Value.Int n -> n
      | _ -> Error.raise_ ~loc Type_mismatch "skip requires integer argument"
    in
    require_collection "skip" input loc;
    xd_compose input (Transducer.skip n)

  (* === Eager collection functions (realize first) === *)
  | "sort_by", [ expr ] ->
    require_collection "sort_by" input loc;
    let xs = Value.to_list_exn (realize_input input) in
    let sorted =
      List.map (fun item -> (eval_per_item env expr item, item)) xs
      |> List.sort (fun (key_a, _) (key_b, _) ->
           Interpreter.value_compare key_a key_b)
      |> List.map snd
    in
    Value.Array sorted
  | "group_by", [ expr ] ->
    require_collection "group_by" input loc;
    let xs = Value.to_list_exn (realize_input input) in
    let groups = Hashtbl.create 16 in
    let order = ref [] in
    List.iter
      (fun item ->
        let key = eval_per_item env expr item in
        let key_str = Interpreter.value_to_string key in
        match Hashtbl.find_opt groups key_str with
        | Some rev_items -> Hashtbl.replace groups key_str (item :: rev_items)
        | None ->
          order := key_str :: !order;
          Hashtbl.add groups key_str [ item ])
      xs;
    Value.object_of_fields
      (List.rev !order
      |> List.map (fun k -> (k, Value.Array (List.rev (Hashtbl.find groups k)))))
  | "reverse", [] ->
    require_collection "reverse" input loc;
    let xs = Value.to_list_exn (realize_input input) in
    Value.Array (List.rev xs)

  (* === Slicing === *)
  | "first", [] -> (
    require_collection "first" input loc;
    let result = match input with
      | Value.Xd (source, xd) ->
        let xd' = Transducer.compose xd (Transducer.take 1) in
        !Value.xd_run_ref xd' (Value.to_seq source)
      | _ ->
        let s = Value.to_seq input in
        (match s () with
         | Seq.Cons (x, _) -> [x]
         | Seq.Nil -> [])
    in
    match result with
    | [x] -> x
    | _ -> Error.raise_ ~loc Runtime_error "first on empty collection")
  | "last", [] ->
    require_collection "last" input loc;
    let xs = Value.to_list_exn (realize_input input) in
    (match xs with
    | [] -> Error.raise_ ~loc Runtime_error "last on empty collection"
    | _ -> List.nth xs (List.length xs - 1))
  | ("count" | "length"), [] -> (
    match input with
    | Value.Array xs -> Value.Int (List.length xs)
    | Value.Xd _ -> Value.Int (fold_collection (fun n _ -> n + 1) 0 input)
    | Value.Seq s -> Value.Int (Seq.fold_left (fun acc _ -> acc + 1) 0 s)
    | Value.Object _ -> Value.Int (Array.length (Value.object_entries input))
    | Value.String s -> Value.Int (String.length s)
    | Value.Null -> Value.Int 0
    | _ -> Value.Int 1)

  (* === Aggregation (fused fold — no materialization) === *)
  | "sum", [] ->
    fold_collection
      (fun acc item ->
        match (acc, item) with
        | Value.Int a, Value.Int b -> Value.Int (a + b)
        | Value.Int a, Value.BigInt b -> Value.of_z (Z.add (Z.of_int a) b)
        | Value.Int a, Value.Float b -> Value.Float (Float.of_int a +. b)
        | Value.BigInt a, Value.Int b -> Value.of_z (Z.add a (Z.of_int b))
        | Value.BigInt a, Value.BigInt b -> Value.of_z (Z.add a b)
        | Value.BigInt a, Value.Float b -> Value.Float (Z.to_float a +. b)
        | Value.Float a, Value.Int b -> Value.Float (a +. Float.of_int b)
        | Value.Float a, Value.BigInt b -> Value.Float (a +. Z.to_float b)
        | Value.Float a, Value.Float b -> Value.Float (a +. b)
        | _ ->
          Error.raise_ ~loc Type_mismatch "sum requires numeric elements")
      (Value.Int 0) input
  | "min", [] -> (
    let items = Value.to_list_exn (realize_input input) in
    match items with
    | [] -> Error.raise_ ~loc Runtime_error "min on empty collection"
    | first :: rest ->
      List.fold_left
        (fun acc item ->
          if Interpreter.value_compare item acc < 0 then item else acc)
        first rest)
  | "max", [] -> (
    let items = Value.to_list_exn (realize_input input) in
    match items with
    | [] -> Error.raise_ ~loc Runtime_error "max on empty collection"
    | first :: rest ->
      List.fold_left
        (fun acc item ->
          if Interpreter.value_compare item acc > 0 then item else acc)
        first rest)
  | "avg", [] ->
    let total, n =
      fold_collection
        (fun (total, n) item -> (total +. Value.to_float_exn item, n + 1))
        (0.0, 0) input
    in
    if n = 0 then Error.raise_ ~loc Runtime_error "avg on empty collection"
    else Value.Float (total /. Float.of_int n)

  (* === String functions === *)
  | "lower", [] -> (
    match input with
    | Value.String s -> Value.String (String.lowercase_ascii s)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "lower requires string, got %s"
           (Value.type_name input)))
  | "upper", [] -> (
    match input with
    | Value.String s -> Value.String (String.uppercase_ascii s)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "upper requires string, got %s"
           (Value.type_name input)))
  | "trim", [] -> (
    match input with
    | Value.String s -> Value.String (String.trim s)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "trim requires string, got %s"
           (Value.type_name input)))
  | "truncate", [ n_expr ] -> (
    let n =
      match eval_in_ctx env input n_expr with
      | Value.Int n -> n
      | _ ->
        Error.raise_ ~loc Type_mismatch "truncate requires integer argument"
    in
    match input with
    | Value.String s ->
      if String.length s <= n then Value.String s
      else Value.String (String.sub s 0 n)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "truncate requires string, got %s"
           (Value.type_name input)))
  | "split", [ sep_expr ] -> (
    let sep =
      match eval_in_ctx env input sep_expr with
      | Value.String s -> s
      | _ -> Error.raise_ ~loc Type_mismatch "split requires string argument"
    in
    match input with
    | Value.String s ->
      Value.Array
        (String.split_on_char (String.get sep 0) s
        |> List.map (fun s -> Value.String s))
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "split requires string, got %s"
           (Value.type_name input)))
  | "join", [ sep_expr ] ->
    let sep =
      match eval_in_ctx env input sep_expr with
      | Value.String s -> s
      | _ -> Error.raise_ ~loc Type_mismatch "join requires string argument"
    in
    require_collection "join" input loc;
    let strs =
      Seq.map
        (fun v ->
          match v with
          | Value.String s -> s
          | other -> Interpreter.value_to_string other)
        (Value.to_seq input)
    in
    Value.String (String.concat sep (List.of_seq strs))

  (* === Object functions === *)
  | "keys", [] -> (
    match input with
    | Value.Object _ ->
      let kvs = Value.object_entries input in
      Value.Array (Array.to_list (Array.map (fun (k, _) -> Value.String k) kvs))
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "keys requires object, got %s"
           (Value.type_name input)))
  | "values", [] -> (
    match input with
    | Value.Object _ ->
      Value.Array (Value.object_entries input |> Array.to_list |> List.map snd)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "values requires object, got %s"
           (Value.type_name input)))
  | "pick", fields ->
    let field_names =
      List.map
        (fun arg ->
          match arg with
          | Ast.Field { name; _ } -> name
          | Ast.Literal (String s) -> s
          | _ ->
            Error.raise_ ~loc Type_mismatch
              "pick requires field names as arguments")
        fields
    in
    Value.object_of_fields
      (List.filter_map
         (fun name ->
           match Value.object_find_opt name input with
           | Some v -> Some (name, v)
           | None -> None)
         field_names)
  | "omit", fields ->
    let field_names =
      List.map
        (fun arg ->
          match arg with
          | Ast.Field { name; _ } -> name
          | Ast.Literal (String s) -> s
          | _ ->
            Error.raise_ ~loc Type_mismatch
              "omit requires field names as arguments")
        fields
    in
    Value.object_of_fields
      (Value.object_entries input
      |> Array.to_list
      |> List.filter (fun (k, _) -> not (List.mem k field_names)))

  (* === Type functions === *)
  | "type", [] -> Value.String (Value.type_name input)
  | "to_string", [] -> Value.String (Interpreter.value_to_string input)
  | "to_number", [] -> (
    match input with
    | Value.Int _ | Value.Float _ -> input
    | Value.BigInt _ -> input
    | Value.String s -> (
      let integer_like =
        let len = String.length s in
        len > 0
        &&
        let rec loop i =
          if i >= len then true
          else
            match s.[i] with
            | '0' .. '9' -> loop (i + 1)
            | _ -> false
        in
        match s.[0] with
        | '-' | '+' -> len > 1 && loop 1
        | '0' .. '9' -> loop 0
        | _ -> false
      in
      if integer_like then
        Value.of_z (Z.of_string s)
      else (
        match float_of_string_opt s with
        | Some f -> Value.Float f
        | None ->
          Error.raise_ ~loc Runtime_error
            (Printf.sprintf "cannot convert %S to number" s)))
    | Value.Bool true -> Value.Int 1
    | Value.Bool false -> Value.Int 0
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "cannot convert %s to number"
           (Value.type_name input)))

  (* === Generators === *)
  | "range", args -> (
    match args with
    | [] ->
      Value.Seq (Seq.ints 0 |> Seq.map (fun i -> Value.Int i))
    | [ end_expr ] ->
      let end_val =
        match eval_in_ctx env input end_expr with
        | Value.Int n -> n
        | _ -> Error.raise_ ~loc Type_mismatch "range requires integer"
      in
      Value.Array (List.init end_val (fun i -> Value.Int i))
    | [ start_expr; end_expr ] ->
      let start_val =
        match eval_in_ctx env input start_expr with
        | Value.Int n -> n
        | _ -> Error.raise_ ~loc Type_mismatch "range requires integer"
      in
      let end_val =
        match eval_in_ctx env input end_expr with
        | Value.Int n -> n
        | _ -> Error.raise_ ~loc Type_mismatch "range requires integer"
      in
      Value.Array
        (List.init (end_val - start_val) (fun i -> Value.Int (start_val + i)))
    | _ -> Error.raise_ ~loc Arity_mismatch "range takes 0-2 arguments")

  | _, [] -> (
    match List.assoc_opt name env with
    | Some v -> v
    | None -> raise_unknown name loc)
  | _ ->
    raise_unknown name loc
