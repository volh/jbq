let top_array_raw_seq json =
  let stream = Simdjson_native.top_array_stream_create json in
  let rec next () =
    match Simdjson_native.top_array_stream_next_raw stream with
    | None -> Seq.Nil
    | Some raw -> Seq.Cons (raw, next)
  in
  next

let top_array_value_seq json =
  let stream = Simdjson_native.top_array_stream_create json in
  let rec next () =
    match Simdjson_native.top_array_stream_next_value stream with
    | None -> Seq.Nil
    | Some value -> Seq.Cons (value, next)
  in
  next

let top_array_input json =
  Value.Seq (top_array_value_seq json)
