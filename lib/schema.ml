let enum_threshold = 20

type schema =
  | SNull
  | SBoolean
  | SInteger
  | SNumber
  | SString
  | SEnum of schema * Value.t list
  | SArray of schema
  | SObject of obj_schema
  | SOneOf of schema list
  | SEmpty

and obj_schema = {
  properties : (string * schema) list;
  required : string list;
  count : int;
}

let base_type = function
  | SEnum (t, _) -> t
  | t -> t

let is_numeric s = match base_type s with SInteger | SNumber -> true | _ -> false

let value_mem v vs =
  List.exists
    (fun v2 ->
      match (v, v2) with
      | Value.String a, Value.String b -> String.equal a b
      | Value.Int a, Value.Int b -> a = b
      | Value.BigInt a, Value.BigInt b -> Z.equal a b
      | Value.Float a, Value.Float b -> Float.equal a b
      | Value.Bool a, Value.Bool b -> a = b
      | Value.Null, Value.Null -> true
      | _ -> false)
    vs

let merge_values vs1 vs2 =
  let combined =
    List.fold_left
      (fun acc v -> if value_mem v acc then acc else acc @ [ v ])
      vs1 vs2
  in
  if List.length combined > enum_threshold then None else Some combined

let rec merge a b =
  match (a, b) with
  | a, b when a = b -> a
  | SEmpty, s | s, SEmpty -> s
  | SEnum (t1, vs1), SEnum (t2, vs2) -> (
    let merged_type = merge t1 t2 in
    match merged_type with
    | SOneOf _ -> merge (SOneOf [ t1; t2 ]) SEmpty
    | _ -> (
      match merge_values vs1 vs2 with
      | Some vs -> SEnum (merged_type, vs)
      | None -> merged_type))
  | SEnum (t, vs), SNull when List.length vs <= enum_threshold ->
    SOneOf [ SEnum (t, vs); SNull ]
  | SNull, SEnum (t, vs) when List.length vs <= enum_threshold ->
    SOneOf [ SEnum (t, vs); SNull ]
  | SInteger, SNumber | SNumber, SInteger -> SNumber
  | SArray ia, SArray ib -> SArray (merge ia ib)
  | SObject oa, SObject ob -> SObject (merge_objects oa ob)
  | SOneOf xs, SOneOf ys ->
    List.fold_left (fun acc y -> merge_into_list acc y) xs ys
    |> normalize_oneof
  | SOneOf xs, s | s, SOneOf xs ->
    merge_into_list xs s |> normalize_oneof
  | SEnum (t, _), other | other, SEnum (t, _) -> merge t other
  | a, b -> SOneOf [ a; b ]

and merge_objects oa ob =
  let all_keys =
    let ka = List.map fst oa.properties in
    let kb = List.map fst ob.properties in
    let seen = Hashtbl.create 16 in
    let result = ref [] in
    List.iter
      (fun k ->
        if not (Hashtbl.mem seen k) then (
          Hashtbl.add seen k ();
          result := k :: !result))
      (ka @ kb);
    List.rev !result
  in
  let properties =
    List.map
      (fun k ->
        let sa = List.assoc_opt k oa.properties in
        let sb = List.assoc_opt k ob.properties in
        let schema =
          match (sa, sb) with
          | Some a, Some b -> merge a b
          | Some a, None -> a
          | None, Some b -> b
          | None, None -> assert false
        in
        (k, schema))
      all_keys
  in
  let required =
    List.filter
      (fun k ->
        List.mem k oa.required && List.mem k ob.required
        && List.mem_assoc k oa.properties
        && List.mem_assoc k ob.properties)
      all_keys
  in
  { properties; required; count = oa.count + ob.count }

and merge_into_list xs s =
  let s_base = base_type s in
  match s with
  | SOneOf ys -> List.fold_left merge_into_list xs ys
  | _ ->
    if List.exists (fun x -> x = s) xs then xs
    else if is_numeric s && List.exists is_numeric xs then
      List.map (fun x -> if is_numeric x then SNumber else x) xs
    else
      let try_merge_compatible () =
        match s_base with
        | SArray sb ->
          let found = ref false in
          let result =
            List.map
              (fun x ->
                match base_type x with
                | SArray sa ->
                  found := true;
                  SArray (merge sa sb)
                | _ -> x)
              xs
          in
          if !found then Some result else None
        | SObject sb ->
          let found = ref false in
          let result =
            List.map
              (fun x ->
                match base_type x with
                | SObject sa ->
                  found := true;
                  SObject (merge_objects sa sb)
                | _ -> x)
              xs
          in
          if !found then Some result else None
        | _ -> None
      in
      let try_merge_enum () =
        match s with
        | SEnum (st, svs) ->
          let found = ref false in
          let result =
            List.map
              (fun x ->
                match x with
                | SEnum (xt, xvs) when base_type (SEnum (xt, xvs)) = st
                                       || (is_numeric (SEnum (xt, xvs))
                                           && is_numeric s) ->
                  found := true;
                  merge (SEnum (xt, xvs)) (SEnum (st, svs))
                | _ -> x)
              xs
          in
          if !found then Some result else None
        | _ -> None
      in
      (match try_merge_enum () with
      | Some result -> result
      | None -> (
        match try_merge_compatible () with
        | Some result -> result
        | None -> xs @ [ s ]))

and normalize_oneof = function
  | [] -> SEmpty
  | [ single ] -> single
  | xs -> SOneOf xs

let all_keys_numeric keys =
  keys <> []
  && List.for_all
       (fun k ->
         String.length k > 0
         &&
         let rec loop i =
           if i >= String.length k then true
           else k.[i] >= '0' && k.[i] <= '9' && loop (i + 1)
         in
         loop 0)
       keys

let merge_all = function
  | [] -> SEmpty
  | first :: rest -> List.fold_left merge first rest

let is_uniform_for_map = function
  | SOneOf xs ->
    let non_null = List.filter (fun s -> s <> SNull) xs in
    List.length non_null <= 1
  | _ -> true

let try_as_map obj =
  let keys = List.map fst obj.properties in
  let values = List.map snd obj.properties in
  let n = List.length keys in
  if n = 0 then None
  else if all_keys_numeric keys then Some (merge_all values)
  else if obj.count > 1 && obj.required = [] && n <= obj.count then
    let merged = merge_all values in
    if is_uniform_for_map merged then Some merged else None
  else None

let rec infer (v : Value.t) : schema =
  match v with
  | Null -> SNull
  | Bool b -> SEnum (SBoolean, [ Bool b ])
  | Int i -> SEnum (SInteger, [ Int i ])
  | BigInt z -> SEnum (SInteger, [ BigInt z ])
  | Float f -> SEnum (SNumber, [ Float f ])
  | String s -> SEnum (SString, [ String s ])
  | Object kvs ->
    let properties = List.map (fun (k, v) -> (k, infer v)) kvs in
    let required = List.map fst kvs in
    SObject { properties; required; count = (if kvs = [] then 0 else 1) }
  | Array [] -> SArray SEmpty
  | Array items -> infer_seq (List.to_seq items)
  | Seq s -> infer_seq s
  | Xd (source, xd) ->
    infer_seq (List.to_seq (!Value.xd_run_ref xd (Value.to_seq_of source)))

and infer_seq seq =
  let item_schema =
    Seq.fold_left
      (fun acc v ->
        let s = infer v in
        match acc with None -> Some s | Some prev -> Some (merge prev s))
      None seq
  in
  match item_schema with None -> SArray SEmpty | Some s -> SArray s

let infer_sampled ~n (v : Value.t) : schema =
  if n <= 0 then infer v
  else
    match v with
    | Array [] -> SArray SEmpty
    | Array items -> infer_seq (List.to_seq items |> Seq.take n)
    | Seq s -> infer_seq (Seq.take n s)
    | Xd (source, xd) ->
      let items = !Value.xd_run_ref xd (Value.to_seq_of source) in
      infer_seq (List.to_seq items |> Seq.take n)
    | _ -> infer v

let is_simple_type = function
  | SNull | SBoolean | SInteger | SNumber | SString -> true
  | _ -> false

let rec to_value (s : schema) : Value.t =
  match s with
  | SNull -> type_obj "null"
  | SBoolean -> type_obj "boolean"
  | SInteger -> type_obj "integer"
  | SNumber -> type_obj "number"
  | SString -> type_obj "string"
  | SEnum (_, [ v ]) -> Value.Object [ ("const", v) ]
  | SEnum (SBoolean, vs)
    when List.length vs = 2
         && value_mem (Value.Bool true) vs
         && value_mem (Value.Bool false) vs ->
    type_obj "boolean"
  | SEnum (ty, vs) ->
    let type_fields =
      match to_value ty with
      | Value.Object kvs -> kvs
      | _ -> []
    in
    Value.Object (type_fields @ [ ("enum", Value.Array vs) ])
  | SEmpty | SArray SEmpty -> type_obj "array"
  | SArray items ->
    Value.Object
      [ ("type", Value.String "array"); ("items", to_value items) ]
  | SObject obj -> (
    match try_as_map obj with
    | Some value_schema ->
      Value.Object
        [
          ("type", Value.String "object");
          ("additionalProperties", to_value value_schema);
        ]
    | None ->
      let props =
        Value.Object
          (List.map (fun (k, s) -> (k, to_value s)) obj.properties)
      in
      let fields =
        [ ("type", Value.String "object"); ("properties", props) ]
      in
      let fields =
        if obj.required <> [] then
          fields
          @ [
              ( "required",
                Value.Array
                  (List.map (fun k -> Value.String k) obj.required) );
            ]
        else fields
      in
      Value.Object fields)
  | SOneOf schemas -> to_value_oneof schemas

and nullable_simple s =
  let type_name =
    match s with
    | SBoolean -> "boolean"
    | SInteger -> "integer"
    | SNumber -> "number"
    | SString -> "string"
    | _ -> "string"
  in
  Value.Object
    [
      ( "type",
        Value.Array [ Value.String type_name; Value.String "null" ] );
    ]

and nullable_enum ty vs =
  let type_name =
    match ty with
    | SBoolean -> "boolean"
    | SInteger -> "integer"
    | SNumber -> "number"
    | SString -> "string"
    | _ -> "string"
  in
  Value.Object
    [
      ( "type",
        Value.Array [ Value.String type_name; Value.String "null" ] );
      ("enum", Value.Array (vs @ [ Value.Null ]));
    ]

and to_value_oneof schemas =
  match schemas with
  | [ s; SNull ] when is_simple_type s -> nullable_simple s
  | [ SNull; s ] when is_simple_type s -> nullable_simple s
  | [ SEnum (ty, vs); SNull ] when is_simple_type ty ->
    nullable_enum ty vs
  | [ SNull; SEnum (ty, vs) ] when is_simple_type ty ->
    nullable_enum ty vs
  | schemas ->
    Value.Object
      [ ("oneOf", Value.Array (List.map to_value schemas)) ]

and type_obj name = Value.Object [ ("type", Value.String name) ]

let add_schema_id (v : Value.t) : Value.t =
  match v with
  | Value.Object kvs ->
    Value.Object
      (( "$schema",
         Value.String "https://json-schema.org/draft/2020-12/schema" )
      :: kvs)
  | _ -> v
