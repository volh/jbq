type resolution_error =
  | Missing_query
  | Duplicate_input of string * string

type resolved = {
  query : string;
  input_source : string option;
}

let resolve ~query ~input_file ~input_opt ~schema =
  let input_source =
    match (input_file, input_opt) with
    | Some a, Some b -> Error (Duplicate_input (a, b))
    | Some path, None | None, Some path -> Ok (Some path)
    | None, None -> Ok None
  in
  match (query, input_source, schema) with
  | _, Error err, _ -> Error err
  | Some q, Ok input_source, _ -> Ok { query = q; input_source }
  | None, Ok input_source, true -> Ok { query = "."; input_source }
  | None, Ok _, false -> Error Missing_query
