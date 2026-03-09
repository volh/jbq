open Ast

let dispatch_ref :
    ((string * Value.t) list -> Value.t -> string -> expr list -> Ast.loc -> Value.t) ref
    =
  ref (fun _ _ _ _ _ -> failwith "stdlib not initialized")

let rec eval (env : (string * Value.t) list) (input : Value.t) (expr : expr) : Value.t =
  match expr with
  | Identity -> input
  | Literal lit -> eval_literal lit
  | Field { name; optional; loc } -> eval_field input name optional loc
  | Index { expr = _; index; loc } -> eval_index input index loc
  | Pipe { left; right } ->
    let mid = eval env input left in
    eval env mid right
  | FnCall { name; args; loc } -> eval_fn env input name args loc
  | BinOp { op; left; right; loc } -> eval_binop env input op left right loc
  | UnaryOp { op = Not; expr; _ } ->
    Bool (not (Value.is_truthy (eval env input expr)))
  | ObjectConstruct { fields; _ } -> eval_object env input fields
  | ArrayConstruct { elements; _ } ->
    Array (List.map (eval env input) elements)
  | If { cond; then_; else_; _ } ->
    if Value.is_truthy (eval env input cond) then eval env input then_
    else (
      match else_ with
      | Some e -> eval env input e
      | None -> Value.Null)
  | Let { name; value; body; _ } ->
    let v = eval env input value in
    eval ((name, v) :: env) input body
  | StringInterp { parts; _ } ->
    let buf = Buffer.create 64 in
    List.iter
      (fun (part : Ast.interp_part) ->
        match part with
        | LitPart s -> Buffer.add_string buf s
        | ExprPart expr ->
          let v = eval env input expr in
          Buffer.add_string buf (value_to_string v))
      parts;
    Value.String (Buffer.contents buf)
  | Lambda _ -> Error.raise_ Runtime_error "lambda cannot be evaluated directly"

and eval_literal = function
  | Null -> Value.Null
  | Bool b -> Value.Bool b
  | Int i -> Value.Int i
  | BigInt z -> Value.of_z z
  | Float f -> Value.Float f
  | String s -> Value.String s

and eval_field input name optional loc =
  match input with
  | Value.Object _ -> (
    match Value.object_find_opt name input with
    | Some v -> v
    | None ->
      if optional then Value.Null
      else
        let available =
          Value.object_entries input |> Array.to_list |> List.map fst
        in
        let suggestion =
          match find_closest name available with
          | Some s -> Some (Printf.sprintf "did you mean .%s?" s)
          | None ->
            Some
              (Printf.sprintf "available keys: %s"
                 (String.concat ", " available))
        in
        Error.raise_ ~loc ?suggestion Key_not_found
          (Printf.sprintf "key \"%s\" not found" name))
  | Value.Null ->
    if optional then Value.Null
    else
      Error.raise_ ~loc Null_access
        (Printf.sprintf "cannot access .%s on null" name)
  | _ ->
    if optional then Value.Null
    else
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "cannot access .%s on %s" name (Value.type_name input))

and eval_index input index loc =
  match index with
  | Single idx_expr ->
    let idx = eval [] input idx_expr in
    (match (input, idx) with
    | Value.Array xs, Value.Int i ->
      let len = List.length xs in
      let i = if i < 0 then len + i else i in
      if i >= 0 && i < len then List.nth xs i
      else
        Error.raise_ ~loc Index_out_of_bounds
          (Printf.sprintf "index %d out of bounds (length %d)" i len)
    | Value.Object _, Value.String k -> (
      match Value.object_find_opt k input with
      | Some v -> v
      | None ->
        Error.raise_ ~loc Key_not_found
          (Printf.sprintf "key \"%s\" not found" k))
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "cannot index %s with %s" (Value.type_name input)
           (Value.type_name idx)))
  | Slice (start_expr, end_expr) ->
    (match input with
    | Value.Array xs ->
      let len = List.length xs in
      let start_i =
        match start_expr with
        | Some e -> (
          match eval [] input e with
          | Value.Int i -> if i < 0 then max 0 (len + i) else i
          | _ -> Error.raise_ ~loc Type_mismatch "slice index must be integer")
        | None -> 0
      in
      let end_i =
        match end_expr with
        | Some e -> (
          match eval [] input e with
          | Value.Int i -> if i < 0 then max 0 (len + i) else min i len
          | _ -> Error.raise_ ~loc Type_mismatch "slice index must be integer")
        | None -> len
      in
      let result =
        xs |> List.to_seq |> Seq.drop start_i
        |> Seq.take (max 0 (end_i - start_i))
        |> List.of_seq
      in
      Value.Array result
    | Value.String s ->
      let len = String.length s in
      let start_i =
        match start_expr with
        | Some e -> (
          match eval [] input e with
          | Value.Int i -> if i < 0 then max 0 (len + i) else i
          | _ -> Error.raise_ ~loc Type_mismatch "slice index must be integer")
        | None -> 0
      in
      let end_i =
        match end_expr with
        | Some e -> (
          match eval [] input e with
          | Value.Int i -> if i < 0 then max 0 (len + i) else min i len
          | _ -> Error.raise_ ~loc Type_mismatch "slice index must be integer")
        | None -> len
      in
      Value.String (String.sub s start_i (max 0 (end_i - start_i)))
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "cannot slice %s" (Value.type_name input)))

and eval_fn env input name args loc =
  !dispatch_ref env input name args loc

and eval_binop env input op left right loc =
  let lv = eval env input left in
  match op with
  | NullCoalesce ->
    if lv = Value.Null then eval env input right else lv
  | And ->
    if Value.is_truthy lv then eval env input right else lv
  | Or ->
    if Value.is_truthy lv then lv else eval env input right
  | _ ->
    let rv = eval env input right in
    eval_binop_values op lv rv loc

and eval_binop_values op lv rv loc =
  match op with
  | Add -> numeric_op Z.add ( +. ) lv rv loc
  | Sub -> numeric_op Z.sub ( -. ) lv rv loc
  | Mul -> numeric_op Z.mul ( *. ) lv rv loc
  | Div -> (
    match int_like_to_z rv with
    | Some z when Z.equal z Z.zero ->
      Error.raise_ ~loc Runtime_error "division by zero"
    | _ -> div_op lv rv loc)
  | Mod -> (
    match (int_like_to_z lv, int_like_to_z rv) with
    | Some _, Some z when Z.equal z Z.zero ->
      Error.raise_ ~loc Runtime_error "modulo by zero"
    | Some a, Some b -> Value.of_z (Z.rem a b)
    | _ -> Error.raise_ ~loc Type_mismatch "% requires integers")
  | Eq -> Value.Bool (value_eq lv rv)
  | Neq -> Value.Bool (not (value_eq lv rv))
  | Lt -> Value.Bool (value_compare lv rv < 0)
  | Gt -> Value.Bool (value_compare lv rv > 0)
  | Lte -> Value.Bool (value_compare lv rv <= 0)
  | Gte -> Value.Bool (value_compare lv rv >= 0)
  | Concat -> (
    match (lv, rv) with
    | Value.String a, Value.String b -> Value.String (a ^ b)
    | Value.String a, other ->
      Value.String (a ^ value_to_string other)
    | other, Value.String b ->
      Value.String (value_to_string other ^ b)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "++ requires at least one string, got %s and %s"
           (Value.type_name lv) (Value.type_name rv)))
  | NullCoalesce | And | Or ->
    (* handled above with short-circuit *)
    assert false

and int_like_to_z = function
  | Value.Int i -> Some (Z.of_int i)
  | Value.BigInt z -> Some z
  | _ -> None

and float_like = function
  | Value.Int i -> Some (Float.of_int i)
  | Value.BigInt z -> Some (Z.to_float z)
  | Value.Float f -> Some f
  | _ -> None

and numeric_op int_op float_op lv rv loc =
  match (int_like_to_z lv, int_like_to_z rv) with
  | Some a, Some b -> Value.of_z (int_op a b)
  | _ -> (
    match (float_like lv, float_like rv) with
    | Some a, Some b -> Value.Float (float_op a b)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "arithmetic requires numbers, got %s and %s"
           (Value.type_name lv) (Value.type_name rv)))

and div_op lv rv loc =
  match (int_like_to_z lv, int_like_to_z rv) with
  | Some a, Some b -> Value.of_z (Z.div a b)
  | _ -> (
    match (float_like lv, float_like rv) with
    | Some _, Some 0.0 -> Error.raise_ ~loc Runtime_error "division by zero"
    | Some a, Some b -> Value.Float (a /. b)
    | _ ->
      Error.raise_ ~loc Type_mismatch
        (Printf.sprintf "arithmetic requires numbers, got %s and %s"
           (Value.type_name lv) (Value.type_name rv)))

and value_eq a b =
  match (a, b) with
  | Value.Null, Value.Null -> true
  | Value.Bool a, Value.Bool b -> a = b
  | Value.Int a, Value.Int b -> a = b
  | Value.BigInt a, Value.BigInt b -> Z.equal a b
  | Value.Int a, Value.BigInt b -> Z.equal (Z.of_int a) b
  | Value.BigInt a, Value.Int b -> Z.equal a (Z.of_int b)
  | Value.Float a, Value.Float b -> Float.equal a b
  | Value.Int a, Value.Float b -> Float.equal (Float.of_int a) b
  | Value.Float a, Value.Int b -> Float.equal a (Float.of_int b)
  | Value.BigInt a, Value.Float b -> Float.equal (Z.to_float a) b
  | Value.Float a, Value.BigInt b -> Float.equal a (Z.to_float b)
  | Value.String a, Value.String b -> String.equal a b
  | Value.Array a, Value.Array b ->
    List.length a = List.length b && List.for_all2 value_eq a b
  | Value.Object a, Value.Object b ->
    Array.length a.fields = Array.length b.fields
    &&
    let rec loop i =
      if i = Array.length a.fields then true
      else
        let k1, v1 = a.fields.(i) in
        let k2, v2 = b.fields.(i) in
        String.equal k1 k2 && value_eq v1 v2 && loop (i + 1)
    in
    loop 0
  | _ -> false

and value_compare a b =
  match (a, b) with
  | Value.Null, Value.Null -> 0
  | Value.Bool a, Value.Bool b -> compare a b
  | Value.Int a, Value.Int b -> compare a b
  | Value.BigInt a, Value.BigInt b -> Z.compare a b
  | Value.Int a, Value.BigInt b -> Z.compare (Z.of_int a) b
  | Value.BigInt a, Value.Int b -> Z.compare a (Z.of_int b)
  | Value.Float a, Value.Float b -> Float.compare a b
  | Value.Int a, Value.Float b -> Float.compare (Float.of_int a) b
  | Value.Float a, Value.Int b -> Float.compare a (Float.of_int b)
  | Value.BigInt a, Value.Float b -> Float.compare (Z.to_float a) b
  | Value.Float a, Value.BigInt b -> Float.compare a (Z.to_float b)
  | Value.String a, Value.String b -> String.compare a b
  | _ -> compare (Value.type_name a) (Value.type_name b)

and value_to_string = function
  | Value.Null -> "null"
  | Value.Bool true -> "true"
  | Value.Bool false -> "false"
  | Value.Int i -> string_of_int i
  | Value.BigInt z -> Z.to_string z
  | Value.Float f ->
    let s = Printf.sprintf "%.17g" f in
    s
  | Value.String s -> s
  | other -> Printer.to_json ~compact:true other

and eval_object env input fields =
  let kvs =
    List.map
      (fun (field : Ast.obj_field) ->
        match field with
        | Punned name -> (
          match List.assoc_opt name env with
          | Some v -> (name, v)
          | None -> (name, eval_field input name false Ast.dummy_loc))
        | Explicit { key; value } -> (key, eval env input value))
      fields
  in
  Value.object_of_fields kvs

and find_closest target candidates =
  let distance a b =
    let len_a = String.length a and len_b = String.length b in
    if len_a = 0 then len_b
    else if len_b = 0 then len_a
    else
      let matrix = Array.make_matrix (len_a + 1) (len_b + 1) 0 in
      for i = 0 to len_a do
        matrix.(i).(0) <- i
      done;
      for j = 0 to len_b do
        matrix.(0).(j) <- j
      done;
      for i = 1 to len_a do
        for j = 1 to len_b do
          let cost = if Char.equal a.[i - 1] b.[j - 1] then 0 else 1 in
          matrix.(i).(j) <-
            min
              (min (matrix.(i - 1).(j) + 1) (matrix.(i).(j - 1) + 1))
              (matrix.(i - 1).(j - 1) + cost)
        done
      done;
      matrix.(len_a).(len_b)
  in
  let scored =
    List.map (fun c -> (c, distance target c)) candidates
    |> List.sort (fun (_, a) (_, b) -> compare a b)
  in
  match scored with
  | (name, dist) :: _ when dist <= 3 -> Some name
  | _ -> None
