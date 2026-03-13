let () =
  Callback.register "jbq_simdjson_object_of_fields" Value.object_of_fields;
  Callback.register "jbq_simdjson_bigint_value_of_string"
    (fun s -> Value.of_z (Z.of_string s))

type top_array_stream

external available : unit -> bool = "jbq_simdjson_available"
external version : unit -> string = "jbq_simdjson_version"
external parse_value : string -> Value.t = "jbq_simdjson_parse_value"

external top_array_stream_create_raw : string -> top_array_stream
  = "jbq_simdjson_top_array_stream_create"

external top_array_stream_next_raw : top_array_stream -> string option
  = "jbq_simdjson_top_array_stream_next_raw"

external top_array_stream_next_value : top_array_stream -> Value.t option
  = "jbq_simdjson_top_array_stream_next_value"

let top_array_stream_create json = top_array_stream_create_raw json

let rec drain_raw stream acc =
  match top_array_stream_next_raw stream with
  | None -> List.rev acc
  | Some raw -> drain_raw stream (raw :: acc)

let top_array_elements_raw json =
  let stream = top_array_stream_create json in
  drain_raw stream []
