let enum_threshold = 20

let object_entries_list v =
  Value.object_entries v
  |> Array.to_list

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

let is_simple_type = function
  | SNull | SBoolean | SInteger | SNumber | SString -> true
  | _ -> false

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

type enum_key =
  | EKNull
  | EKBool of bool
  | EKInt of int
  | EKBigInt of string
  | EKFloat of int64
  | EKString of string

let enum_key_of_value = function
  | Value.Null -> EKNull
  | Value.Bool b -> EKBool b
  | Value.Int i -> EKInt i
  | Value.BigInt z -> EKBigInt (Z.to_string z)
  | Value.Float f -> EKFloat (Int64.bits_of_float f)
  | Value.String s -> EKString s
  | _ -> invalid_arg "enum_key_of_value expects scalar value"

let merge_values vs1 vs2 =
  let seen = Hashtbl.create (List.length vs1 + List.length vs2) in
  let rev_values = ref [] in
  let count = ref 0 in
  let add v =
    if !count <= enum_threshold then
      let key = enum_key_of_value v in
      if not (Hashtbl.mem seen key) then (
        Hashtbl.add seen key ();
        incr count;
        rev_values := v :: !rev_values)
  in
  List.iter add vs1;
  List.iter add vs2;
  if !count > enum_threshold then None else Some (List.rev !rev_values)

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
  let property_table properties =
    let tbl = Hashtbl.create (List.length properties) in
    List.iter (fun (k, v) -> Hashtbl.replace tbl k v) properties;
    tbl
  in
  let string_set values =
    let tbl = Hashtbl.create (List.length values) in
    List.iter (fun v -> Hashtbl.replace tbl v ()) values;
    tbl
  in
  let props_a = property_table oa.properties in
  let props_b = property_table ob.properties in
  let req_a = string_set oa.required in
  let req_b = string_set ob.required in
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
        let sa = Hashtbl.find_opt props_a k in
        let sb = Hashtbl.find_opt props_b k in
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
        Hashtbl.mem req_a k && Hashtbl.mem req_b k
        && Hashtbl.mem props_a k
        && Hashtbl.mem props_b k)
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
    else if is_simple_type s
            && List.exists
                 (fun x ->
                   match x with SEnum (t, _) when t = s -> true | _ -> false)
                 xs
    then
      List.map
        (fun x -> match x with SEnum (t, _) when t = s -> s | _ -> x)
        xs
    else if (match s with
            | SEnum (t, _) when is_simple_type t -> List.mem t xs
            | _ -> false)
    then xs
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

and normalize_oneof xs =
  let dominated x =
    match x with
    | SEnum (t, _) when is_simple_type t ->
      List.exists (fun y -> y = t) xs
    | _ -> false
  in
  match List.filter (fun x -> not (dominated x)) xs with
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
  | Object _ ->
    let kvs = object_entries_list v in
    let properties = List.map (fun (k, v) -> (k, infer v)) kvs in
    let required = List.map fst kvs in
    SObject { properties; required; count = (if kvs = [] then 0 else 1) }
  | Array [] -> SArray SEmpty
  | Array items -> infer_seq (List.to_seq items)
  | Seq s -> infer_seq s
  | Xd (source, xd) ->
    infer_seq (List.to_seq (!Value.xd_run_ref xd (Value.to_seq_of source)))

and infer_seq seq =
  let merge_inferred acc v =
    let s = infer v in
    match acc with None -> Some s | Some prev -> Some (merge prev s)
  in
  let item_schema =
    Seq.fold_left merge_inferred None seq
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
      let limited = Transducer.compose xd (Transducer.take n) in
      let merge_inferred acc v =
        let s = infer v in
        match acc with None -> Some s | Some prev -> Some (merge prev s)
      in
      let item_schema =
        Transducer.fold limited merge_inferred None (Value.to_seq_of source)
      in
      (match item_schema with None -> SArray SEmpty | Some s -> SArray s)
    | _ -> infer v

let rec strip_const = function
  | SEnum (ty, [ _ ]) -> strip_const ty
  | SArray s -> SArray (strip_const s)
  | SObject obj ->
    SObject
      {
        obj with
        properties =
          List.map (fun (k, s) -> (k, strip_const s)) obj.properties;
      }
  | SOneOf xs -> SOneOf (List.map strip_const xs)
  | s -> s

let rec strip_enum = function
  | SEnum (ty, _) -> strip_enum ty
  | SArray s -> SArray (strip_enum s)
  | SObject obj ->
    SObject
      {
        obj with
        properties =
          List.map (fun (k, s) -> (k, strip_enum s)) obj.properties;
      }
  | SOneOf xs -> SOneOf (List.map strip_enum xs)
  | s -> s

let rec to_value (s : schema) : Value.t =
  match s with
  | SNull -> type_obj "null"
  | SBoolean -> type_obj "boolean"
  | SInteger -> type_obj "integer"
  | SNumber -> type_obj "number"
  | SString -> type_obj "string"
  | SEnum (_, [ v ]) -> Value.object_of_fields [ ("const", v) ]
  | SEnum (SBoolean, vs)
    when List.length vs = 2
         && value_mem (Value.Bool true) vs
         && value_mem (Value.Bool false) vs ->
    type_obj "boolean"
  | SEnum (ty, vs) ->
    let type_fields =
      match to_value ty with
      | Value.Object _ as obj -> object_entries_list obj
      | _ -> []
    in
    Value.object_of_fields (type_fields @ [ ("enum", Value.Array vs) ])
  | SEmpty | SArray SEmpty -> type_obj "array"
  | SArray items ->
    Value.object_of_fields
      [ ("type", Value.String "array"); ("items", to_value items) ]
  | SObject obj -> (
    match try_as_map obj with
    | Some value_schema ->
      Value.object_of_fields
        [
          ("type", Value.String "object");
          ("additionalProperties", to_value value_schema);
        ]
    | None ->
      let props =
        Value.object_of_fields
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
      Value.object_of_fields fields)
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
  Value.object_of_fields
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
  Value.object_of_fields
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
    Value.object_of_fields
      [ ("oneOf", Value.Array (List.map to_value schemas)) ]

and type_obj name = Value.object_of_fields [ ("type", Value.String name) ]

let rec normalize_value ?(sort_array = false) = function
  | Value.Object _ as obj ->
    let kvs = object_entries_list obj in
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) kvs in
    Value.object_of_fields
      (List.map
         (fun (k, v) ->
           (k, normalize_value ~sort_array:(k = "required") v))
         sorted)
  | Value.Array items ->
    let items = List.map (normalize_value ~sort_array:false) items in
    if sort_array then
      Value.Array
        (List.sort
           (fun a b ->
             String.compare
               (Printer.to_json ~compact:true a)
               (Printer.to_json ~compact:true b))
           items)
    else Value.Array items
  | v -> v

let dedup (root : Value.t) : Value.t =
  let tbl = Hashtbl.create 64 in
  let min_size = 50 in
  let canonical v = Printer.to_json ~compact:true (normalize_value v) in
  let rec collect parent_key v =
    match v with
    | Value.Object _ as obj -> (
      let kvs = object_entries_list obj in
      let has_props =
        List.exists (fun (k, _) -> k = "properties") kvs
      in
      if has_props then (
        let ser = canonical v in
        if String.length ser >= min_size then (
          let entry =
            match Hashtbl.find_opt tbl ser with
            | Some (name, count) -> (name, count + 1)
            | None -> (parent_key, 1)
          in
          Hashtbl.replace tbl ser entry));
      List.iter (fun (k, child) -> collect k child) kvs)
    | Value.Array items -> List.iter (collect parent_key) items
    | _ -> ()
  in
  collect "Root" root;
  let defs = Hashtbl.create 16 in
  let names_used = Hashtbl.create 16 in
  let ref_map = Hashtbl.create 16 in
  Hashtbl.iter
    (fun ser (name, count) ->
      if count >= 2 then (
        let base =
          String.capitalize_ascii
            (if String.length name > 0 then name else "Schema")
        in
        let final_name =
          if Hashtbl.mem names_used base then (
            let i = ref 2 in
            while Hashtbl.mem names_used (base ^ string_of_int !i) do
              incr i
            done;
            base ^ string_of_int !i)
          else base
        in
        Hashtbl.replace names_used final_name ();
        Hashtbl.replace ref_map ser final_name;
        Hashtbl.replace defs final_name ser))
    tbl;
  if Hashtbl.length defs = 0 then root
  else
    let rec rewrite v =
      match v with
      | Value.Object _ as obj -> (
        let kvs = object_entries_list obj in
        let has_props =
          List.exists (fun (k, _) -> k = "properties") kvs
        in
        if has_props then
          let ser = canonical v in
          match Hashtbl.find_opt ref_map ser with
          | Some name ->
            Value.object_of_fields
              [ ("$ref", Value.String ("#/$defs/" ^ name)) ]
          | None ->
            Value.object_of_fields
              (List.map (fun (k, child) -> (k, rewrite child)) kvs)
        else
          Value.object_of_fields
            (List.map (fun (k, child) -> (k, rewrite child)) kvs))
      | Value.Array items ->
        Value.Array (List.map rewrite items)
      | _ -> v
    in
    let rewritten = rewrite root in
    let rewrite_children = function
      | Value.Object _ as obj ->
        Value.object_of_fields
          (List.map (fun (k, child) -> (k, rewrite child)) (object_entries_list obj))
      | v -> v
    in
    let defs_value =
      Value.object_of_fields
        (Hashtbl.fold
           (fun name ser acc ->
             let parsed =
               Simdjson_native.parse_value ser
             in
             (name, rewrite_children parsed) :: acc)
           defs [])
    in
    match rewritten with
    | Value.Object _ as obj ->
      Value.object_of_fields
        (("$defs", defs_value) :: object_entries_list obj)
    | _ -> rewritten

let add_schema_id (v : Value.t) : Value.t =
  match v with
  | Value.Object _ as obj ->
    Value.object_of_fields
      (( "$schema",
         Value.String "https://json-schema.org/draft/2020-12/schema" )
      :: object_entries_list obj)
  | _ -> v
