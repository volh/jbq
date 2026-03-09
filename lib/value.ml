type xd_signal = Xd_continue | Xd_done

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of object_data
  | Seq of t Seq.t
  | Xd of t * xd
  | BigInt of Z.t

and object_data = {
  fields : (string * t) array;
}

and xd_step = t list -> t -> t list * xd_signal
and xd = { xd_init : xd_step -> xd_step }

let type_name = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | BigInt _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Seq _ | Xd _ -> "sequence"

let is_truthy = function
  | Null -> false
  | Bool b -> b
  | Int 0 -> false
  | BigInt z when Z.equal z Z.zero -> false
  | Float 0.0 -> false
  | String "" -> false
  | Array [] -> false
  | _ -> true

let xd_run_ref : (xd -> t Seq.t -> t list) ref =
  ref (fun _ _ -> failwith "transducer runtime not initialized")

let rec to_seq_of : t -> t Seq.t = function
  | Array xs -> List.to_seq xs
  | Seq s -> s
  | Xd (source, xd) -> List.to_seq (!xd_run_ref xd (to_seq_of source))
  | v -> failwith ("expected collection, got " ^ type_name v)

let of_z z =
  if Z.fits_int z then Int (Z.to_int z) else BigInt z

let to_float_exn = function
  | Int i -> Float.of_int i
  | BigInt z -> Z.to_float z
  | Float f -> f
  | v -> failwith ("expected number, got " ^ type_name v)

let to_string_exn = function
  | String s -> s
  | v -> failwith ("expected string, got " ^ type_name v)

let object_of_fields fields = Object { fields = Array.of_list fields }

let object_entries = function
  | Object obj -> obj.fields
  | v -> failwith ("expected object, got " ^ type_name v)

let object_find_opt key = function
  | Object obj ->
    let rec loop i =
      if i >= Array.length obj.fields then None
      else
        let k, v = obj.fields.(i) in
        if String.equal k key then Some v else loop (i + 1)
    in
    loop 0
  | v -> failwith ("expected object, got " ^ type_name v)

let to_list_exn = function
  | Array xs -> xs
  | Seq s -> List.of_seq s
  | Xd (source, xd) -> !xd_run_ref xd (to_seq_of source)
  | v -> failwith ("expected array, got " ^ type_name v)

let realize = function
  | Seq s -> Array (List.of_seq s)
  | Xd (source, xd) -> Array (!xd_run_ref xd (to_seq_of source))
  | v -> v

let to_seq = to_seq_of

let is_seq = function Seq _ | Xd _ -> true | _ -> false
let is_collection = function Array _ | Seq _ | Xd _ -> true | _ -> false
