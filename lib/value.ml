type xd_signal = Xd_continue | Xd_done

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list
  | Seq of t Seq.t
  | Xd of t * xd

and xd_step = t list -> t -> t list * xd_signal
and xd = { xd_init : xd_step -> xd_step }

let type_name = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Seq _ | Xd _ -> "sequence"

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

let xd_run_ref : (xd -> t Seq.t -> t list) ref =
  ref (fun _ _ -> failwith "transducer runtime not initialized")

let rec to_seq_of : t -> t Seq.t = function
  | Array xs -> List.to_seq xs
  | Seq s -> s
  | Xd (source, xd) -> List.to_seq (!xd_run_ref xd (to_seq_of source))
  | v -> failwith ("expected collection, got " ^ type_name v)

let rec to_yojson : t -> Yojson.Basic.t = function
  | Null -> `Null
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Float f -> `Float f
  | String s -> `String s
  | Array xs -> `List (List.map to_yojson xs)
  | Object kvs -> `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) kvs)
  | Seq s -> `List (List.map to_yojson (List.of_seq s))
  | Xd (source, xd) ->
    `List (List.map to_yojson (!xd_run_ref xd (to_seq_of source)))

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
  | Xd (source, xd) -> !xd_run_ref xd (to_seq_of source)
  | v -> failwith ("expected array, got " ^ type_name v)

let to_assoc_exn = function
  | Object kvs -> kvs
  | v -> failwith ("expected object, got " ^ type_name v)

let realize = function
  | Seq s -> Array (List.of_seq s)
  | Xd (source, xd) -> Array (!xd_run_ref xd (to_seq_of source))
  | v -> v

let to_seq = to_seq_of

let is_seq = function Seq _ | Xd _ -> true | _ -> false
let is_collection = function Array _ | Seq _ | Xd _ -> true | _ -> false
