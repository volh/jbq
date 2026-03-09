type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list
  | Seq of t Seq.t

let type_name = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Seq _ -> "sequence"

let is_truthy = function
  | Null -> false
  | Bool b -> b
  | Int 0 -> false
  | Float 0.0 -> false
  | String "" -> false
  | Array [] -> false
  | _ -> true

let rec of_yojson : Yojson.Basic.t -> t = function
  | `Null -> Null
  | `Bool b -> Bool b
  | `Int i -> Int i
  | `Float f -> Float f
  | `String s -> String s
  | `List xs -> Array (List.map of_yojson xs)
  | `Assoc kvs -> Object (List.map (fun (k, v) -> (k, of_yojson v)) kvs)

let rec to_yojson : t -> Yojson.Basic.t = function
  | Null -> `Null
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Float f -> `Float f
  | String s -> `String s
  | Array xs -> `List (List.map to_yojson xs)
  | Object kvs -> `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) kvs)
  | Seq s -> `List (List.map to_yojson (List.of_seq s))

let to_number_exn v =
  match v with
  | Int i -> Either.Left i
  | Float f -> Either.Right f
  | _ -> failwith ("expected number, got " ^ type_name v)

let to_float_exn = function
  | Int i -> Float.of_int i
  | Float f -> f
  | v -> failwith ("expected number, got " ^ type_name v)

let to_string_exn = function
  | String s -> s
  | v -> failwith ("expected string, got " ^ type_name v)

let to_list_exn = function
  | Array xs -> xs
  | Seq s -> List.of_seq s
  | v -> failwith ("expected array, got " ^ type_name v)

let to_assoc_exn = function
  | Object kvs -> kvs
  | v -> failwith ("expected object, got " ^ type_name v)

let realize = function Seq s -> Array (List.of_seq s) | v -> v
