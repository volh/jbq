open Ast

let eval_in_ctx env input expr = Interpreter.eval env input expr

let as_list input =
  match input with
  | Value.Array xs -> xs
  | Value.Seq s -> List.of_seq s
  | _ -> [ input ]

let implicit_iter input f =
  (* Implicit iteration: when input is an array, iterate over elements *)
  match input with
  | Value.Array xs -> Value.Array (f xs)
  | Value.Seq s -> Value.Array (f (List.of_seq s))
  | _ -> Error.raise_ Type_mismatch
           (Printf.sprintf "expected array, got %s" (Value.type_name input))

let dispatch env input name args loc =
  match (name, args) with
  (* === Collection functions === *)
  | "where", [ pred ] ->
    implicit_iter input (fun xs ->
        List.filter (fun item -> Value.is_truthy (eval_in_ctx env item pred)) xs)
  | "map", [ expr ] ->
    implicit_iter input (fun xs ->
        List.map (fun item -> eval_in_ctx env item expr) xs)
  | "sort_by", [ expr ] ->
    implicit_iter input (fun xs ->
        List.sort
          (fun a b ->
            Interpreter.value_compare
              (eval_in_ctx env a expr)
              (eval_in_ctx env b expr))
          xs)
  | "group_by", [ expr ] ->
    let xs =
      match input with
      | Value.Array xs -> xs
      | Value.Seq s -> List.of_seq s
      | _ ->
        Error.raise_ ~loc Type_mismatch
          (Printf.sprintf "group_by requires array, got %s"
             (Value.type_name input))
    in
    let groups = Hashtbl.create 16 in
    let order = ref [] in
    List.iter
      (fun item ->
        let key = eval_in_ctx env item expr in
        let key_str = Interpreter.value_to_string key in
        if not (Hashtbl.mem groups key_str) then
          order := key_str :: !order;
        let existing =
          match Hashtbl.find_opt groups key_str with
          | Some xs -> xs
          | None -> []
        in
        Hashtbl.replace groups key_str (existing @ [ item ]))
      xs;
    Value.Object
      (List.rev !order
      |> List.map (fun k -> (k, Value.Array (Hashtbl.find groups k))))
  | "unique", [] ->
    implicit_iter input (fun xs ->
        let seen = Hashtbl.create 16 in
        List.filter
          (fun item ->
            let s = Yojson.Basic.to_string (Value.to_yojson item) in
            if Hashtbl.mem seen s then false
            else (
              Hashtbl.replace seen s ();
              true))
          xs)
  | "flatten", [] ->
    implicit_iter input (fun xs ->
        List.concat_map
          (fun item ->
            match item with Value.Array inner -> inner | _ -> [ item ])
          xs)
  | "reverse", [] ->
    implicit_iter input (fun xs -> List.rev xs)

  (* === Slicing === *)
  | "first", [] -> (
    match input with
    | Value.Array (x :: _) -> x
    | Value.Array [] ->
      Error.raise_ ~loc Runtime_error "first on empty array"
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "first requires array, got %s"
           (Value.type_name input)))
  | "last", [] -> (
    match input with
    | Value.Array xs when List.length xs > 0 ->
      List.nth xs (List.length xs - 1)
    | Value.Array [] ->
      Error.raise_ ~loc Runtime_error "last on empty array"
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "last requires array, got %s"
           (Value.type_name input)))
  | "take", [ n_expr ] -> (
    let n =
      match eval_in_ctx env input n_expr with
      | Value.Int n -> n
      | _ -> Error.raise_ ~loc Type_mismatch "take requires integer argument"
    in
    match input with
    | Value.Array xs ->
      Value.Array (List.to_seq xs |> Seq.take n |> List.of_seq)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "take requires array, got %s"
           (Value.type_name input)))
  | "skip", [ n_expr ] -> (
    let n =
      match eval_in_ctx env input n_expr with
      | Value.Int n -> n
      | _ -> Error.raise_ ~loc Type_mismatch "skip requires integer argument"
    in
    match input with
    | Value.Array xs ->
      Value.Array (List.to_seq xs |> Seq.drop n |> List.of_seq)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "skip requires array, got %s"
           (Value.type_name input)))
  | "count", [] -> (
    match input with
    | Value.Array xs -> Value.Int (List.length xs)
    | Value.Object kvs -> Value.Int (List.length kvs)
    | Value.String s -> Value.Int (String.length s)
    | Value.Null -> Value.Int 0
    | _ -> Value.Int 1)
  | "length", [] -> (
    match input with
    | Value.Array xs -> Value.Int (List.length xs)
    | Value.Object kvs -> Value.Int (List.length kvs)
    | Value.String s -> Value.Int (String.length s)
    | Value.Null -> Value.Int 0
    | _ -> Value.Int 1)

  (* === Aggregation === *)
  | "sum", [] ->
    let xs = as_list input in
    List.fold_left
      (fun acc item ->
        match (acc, item) with
        | Value.Int a, Value.Int b -> Value.Int (a + b)
        | Value.Int a, Value.Float b -> Value.Float (Float.of_int a +. b)
        | Value.Float a, Value.Int b -> Value.Float (a +. Float.of_int b)
        | Value.Float a, Value.Float b -> Value.Float (a +. b)
        | _ ->
          Error.raise_ ~loc Type_mismatch "sum requires numeric elements")
      (Value.Int 0) xs
  | "min", [] -> (
    let xs = as_list input in
    match xs with
    | [] -> Error.raise_ ~loc Runtime_error "min on empty array"
    | _ ->
      List.fold_left
        (fun acc item ->
          if Interpreter.value_compare item acc < 0 then item else acc)
        (List.hd xs) (List.tl xs))
  | "max", [] -> (
    let xs = as_list input in
    match xs with
    | [] -> Error.raise_ ~loc Runtime_error "max on empty array"
    | _ ->
      List.fold_left
        (fun acc item ->
          if Interpreter.value_compare item acc > 0 then item else acc)
        (List.hd xs) (List.tl xs))
  | "avg", [] ->
    let xs = as_list input in
    let n = List.length xs in
    if n = 0 then Error.raise_ ~loc Runtime_error "avg on empty array"
    else
      let total =
        List.fold_left
          (fun acc item -> acc +. Value.to_float_exn item)
          0.0 xs
      in
      Value.Float (total /. Float.of_int n)

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
  | "join", [ sep_expr ] -> (
    let sep =
      match eval_in_ctx env input sep_expr with
      | Value.String s -> s
      | _ -> Error.raise_ ~loc Type_mismatch "join requires string argument"
    in
    match input with
    | Value.Array xs ->
      let strs =
        List.map
          (fun v ->
            match v with
            | Value.String s -> s
            | other -> Interpreter.value_to_string other)
          xs
      in
      Value.String (String.concat sep strs)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "join requires array, got %s"
           (Value.type_name input)))

  (* === Object functions === *)
  | "keys", [] -> (
    match input with
    | Value.Object kvs ->
      Value.Array (List.map (fun (k, _) -> Value.String k) kvs)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "keys requires object, got %s"
           (Value.type_name input)))
  | "values", [] -> (
    match input with
    | Value.Object kvs -> Value.Array (List.map snd kvs)
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
    let kvs = Value.to_assoc_exn input in
    Value.Object
      (List.filter_map
         (fun name ->
           match List.assoc_opt name kvs with
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
    let kvs = Value.to_assoc_exn input in
    Value.Object
      (List.filter (fun (k, _) -> not (List.mem k field_names)) kvs)

  (* === Type functions === *)
  | "type", [] -> Value.String (Value.type_name input)
  | "to_string", [] -> Value.String (Interpreter.value_to_string input)
  | "to_number", [] -> (
    match input with
    | Value.Int _ | Value.Float _ -> input
    | Value.String s -> (
      match int_of_string_opt s with
      | Some i -> Value.Int i
      | None -> (
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
      (* infinite range from 0 *)
      Value.Seq (Seq.ints 0 |> Seq.map (fun i -> Value.Int i))
    | [ end_expr ] ->
      let end_val =
        match eval_in_ctx env input end_expr with
        | Value.Int n -> n
        | _ -> Error.raise_ ~loc Type_mismatch "range requires integer"
      in
      Value.Array
        (List.init end_val (fun i -> Value.Int i))
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

  | _ ->
    let all_fns =
      [
        "where"; "map"; "sort_by"; "group_by"; "unique"; "flatten"; "reverse";
        "first"; "last"; "take"; "skip"; "count"; "length";
        "sum"; "min"; "max"; "avg";
        "lower"; "upper"; "trim"; "truncate"; "split"; "join";
        "keys"; "values"; "pick"; "omit";
        "type"; "to_string"; "to_number";
        "range";
      ]
    in
    let suggestion = Interpreter.find_closest name all_fns in
    Error.raise_ ~loc
      ?suggestion:
        (match suggestion with
        | Some s -> Some (Printf.sprintf "did you mean %s?" s)
        | None ->
          Some
            (Printf.sprintf "available functions: %s"
               (String.concat ", " all_fns)))
      Unknown_function
      (Printf.sprintf "unknown function: %s" name)
