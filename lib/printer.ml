let to_json ?(compact = false) (value : Value.t) : string =
  let yojson = Value.to_yojson (Value.realize value) in
  if compact then Yojson.Basic.to_string yojson
  else Yojson.Basic.pretty_to_string yojson
