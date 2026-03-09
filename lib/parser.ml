type state = {
  tokens : Lexer.positioned_token array;
  mutable pos : int;
  source : string;
}

let make source =
  let tokens = Lexer.tokenize source |> Array.of_list in
  { tokens; pos = 0; source }

let current st =
  if st.pos < Array.length st.tokens then st.tokens.(st.pos)
  else { Lexer.token = EOF; pos = String.length st.source }

let peek st = (current st).token

let advance st =
  let t = current st in
  st.pos <- st.pos + 1;
  t

let expect st expected =
  let t = advance st in
  if t.token <> expected then
    Error.raise_
      ~loc:{ start_pos = t.pos; end_pos = t.pos + 1 }
      Parse_error
      (Printf.sprintf "expected %s, got %s"
         (Lexer.token_to_string expected)
         (Lexer.token_to_string t.token))

let current_pos st = (current st).pos

let loc_from start_pos st =
  Ast.{ start_pos; end_pos = current_pos st }

(* Precedence levels, low to high:
   pipe, or, and, equality, comparison, null_coalesce,
   concat, additive, multiplicative, unary, postfix, primary *)

let rec parse_expr st = parse_pipe st

and parse_pipe st =
  let left = parse_or st in
  match peek st with
  | Lexer.PIPE ->
    ignore (advance st);
    let right = parse_pipe st in
    Ast.Pipe { left; right }
  | _ -> left

and parse_or st =
  let left = parse_and st in
  match peek st with
  | Lexer.OR ->
    let start = current_pos st in
    ignore (advance st);
    let right = parse_or st in
    Ast.BinOp { op = Or; left; right; loc = loc_from start st }
  | _ -> left

and parse_and st =
  let left = parse_equality st in
  match peek st with
  | Lexer.AND ->
    let start = current_pos st in
    ignore (advance st);
    let right = parse_and st in
    Ast.BinOp { op = And; left; right; loc = loc_from start st }
  | _ -> left

and parse_equality st =
  let left = parse_comparison st in
  match peek st with
  | Lexer.EQ ->
    let start = current_pos st in
    ignore (advance st);
    let right = parse_comparison st in
    Ast.BinOp { op = Eq; left; right; loc = loc_from start st }
  | Lexer.NEQ ->
    let start = current_pos st in
    ignore (advance st);
    let right = parse_comparison st in
    Ast.BinOp { op = Neq; left; right; loc = loc_from start st }
  | _ -> left

and parse_comparison st =
  let left = parse_null_coalesce st in
  let start = current_pos st in
  match peek st with
  | Lexer.LT ->
    ignore (advance st);
    let right = parse_null_coalesce st in
    Ast.BinOp { op = Lt; left; right; loc = loc_from start st }
  | Lexer.GT ->
    ignore (advance st);
    let right = parse_null_coalesce st in
    Ast.BinOp { op = Gt; left; right; loc = loc_from start st }
  | Lexer.LTE ->
    ignore (advance st);
    let right = parse_null_coalesce st in
    Ast.BinOp { op = Lte; left; right; loc = loc_from start st }
  | Lexer.GTE ->
    ignore (advance st);
    let right = parse_null_coalesce st in
    Ast.BinOp { op = Gte; left; right; loc = loc_from start st }
  | _ -> left

and parse_null_coalesce st =
  let left = parse_concat st in
  match peek st with
  | Lexer.NULL_COALESCE ->
    let start = current_pos st in
    ignore (advance st);
    let right = parse_null_coalesce st in
    Ast.BinOp { op = NullCoalesce; left; right; loc = loc_from start st }
  | _ -> left

and parse_concat st =
  let left = parse_additive st in
  match peek st with
  | Lexer.CONCAT ->
    let start = current_pos st in
    ignore (advance st);
    let right = parse_concat st in
    Ast.BinOp { op = Concat; left; right; loc = loc_from start st }
  | _ -> left

and parse_additive st =
  let left = parse_multiplicative st in
  let start = current_pos st in
  match peek st with
  | Lexer.PLUS ->
    ignore (advance st);
    let right = parse_additive st in
    Ast.BinOp { op = Add; left; right; loc = loc_from start st }
  | Lexer.MINUS ->
    ignore (advance st);
    let right = parse_additive st in
    Ast.BinOp { op = Sub; left; right; loc = loc_from start st }
  | _ -> left

and parse_multiplicative st =
  let left = parse_unary st in
  let start = current_pos st in
  match peek st with
  | Lexer.STAR ->
    ignore (advance st);
    let right = parse_multiplicative st in
    Ast.BinOp { op = Mul; left; right; loc = loc_from start st }
  | Lexer.SLASH ->
    ignore (advance st);
    let right = parse_multiplicative st in
    Ast.BinOp { op = Div; left; right; loc = loc_from start st }
  | Lexer.PERCENT ->
    ignore (advance st);
    let right = parse_multiplicative st in
    Ast.BinOp { op = Mod; left; right; loc = loc_from start st }
  | _ -> left

and parse_unary st =
  match peek st with
  | Lexer.BANG ->
    let start = current_pos st in
    ignore (advance st);
    let expr = parse_unary st in
    Ast.UnaryOp { op = Not; expr; loc = loc_from start st }
  | Lexer.MINUS ->
    let start = current_pos st in
    ignore (advance st);
    let expr = parse_unary st in
    Ast.BinOp
      { op = Sub; left = Ast.Literal (Int 0); right = expr; loc = loc_from start st }
  | _ -> parse_postfix st

and parse_postfix st =
  let expr = parse_primary st in
  parse_postfix_chain expr st

and parse_postfix_chain expr st =
  match peek st with
  | Lexer.DOT -> (
    ignore (advance st);
    match peek st with
    | Lexer.IDENT name ->
      let start = current_pos st in
      ignore (advance st);
      let optional = peek st = Lexer.QUESTION in
      if optional then ignore (advance st);
      let field =
        Ast.Pipe
          {
            left = expr;
            right = Field { name; optional; loc = loc_from start st };
          }
      in
      parse_postfix_chain field st
    | Lexer.LBRACKET ->
      let index_expr = parse_index st in
      let chained = Ast.Pipe { left = expr; right = index_expr } in
      parse_postfix_chain chained st
    | _ ->
      Error.raise_
        ~loc:{ start_pos = current_pos st; end_pos = current_pos st + 1 }
        Parse_error "expected field name or [ after .")
  | Lexer.LBRACKET ->
    let index_expr = parse_index st in
    let chained = Ast.Pipe { left = expr; right = index_expr } in
    parse_postfix_chain chained st
  | Lexer.QUESTION ->
    (* postfix ? on function calls — handled in interpreter *)
    ignore (advance st);
    parse_postfix_chain expr st
  | _ -> expr

and parse_index st =
  let start = current_pos st in
  expect st Lexer.LBRACKET;
  match peek st with
  | Lexer.COLON ->
    ignore (advance st);
    let end_expr =
      if peek st = Lexer.RBRACKET then None
      else Some (parse_expr st)
    in
    expect st Lexer.RBRACKET;
    Ast.Index { expr = Identity; index = Slice (None, end_expr); loc = loc_from start st }
  | Lexer.RBRACKET ->
    ignore (advance st);
    Ast.Index { expr = Identity; index = Slice (None, None); loc = loc_from start st }
  | _ ->
    let idx = parse_expr st in
    if peek st = Lexer.COLON then (
      ignore (advance st);
      let end_expr =
        if peek st = Lexer.RBRACKET then None
        else Some (parse_expr st)
      in
      expect st Lexer.RBRACKET;
      Ast.Index
        { expr = Identity; index = Slice (Some idx, end_expr); loc = loc_from start st })
    else (
      expect st Lexer.RBRACKET;
      Ast.Index { expr = Identity; index = Single idx; loc = loc_from start st })

and parse_primary st =
  let t = current st in
  match t.token with
  | Lexer.DOT -> (
    ignore (advance st);
    match peek st with
    | Lexer.IDENT name ->
      let start = current_pos st in
      ignore (advance st);
      let optional = peek st = Lexer.QUESTION in
      if optional then ignore (advance st);
      Ast.Field { name; optional; loc = loc_from start st }
    | Lexer.LBRACKET -> parse_index st
    | _ -> Ast.Identity)
  | Lexer.INT i ->
    ignore (advance st);
    Ast.Literal (Int i)
  | Lexer.FLOAT f ->
    ignore (advance st);
    Ast.Literal (Float f)
  | Lexer.STRING s ->
    ignore (advance st);
    Ast.Literal (String s)
  | Lexer.INTERP_STRING parts ->
    let start = current_pos st in
    ignore (advance st);
    let ast_parts =
      List.map
        (fun (part : Lexer.interp_part) ->
          match part with
          | Lit s -> Ast.LitPart s
          | Expr src ->
            let sub_st = make src in
            let expr = parse_expr sub_st in
            Ast.ExprPart expr)
        parts
    in
    Ast.StringInterp { parts = ast_parts; loc = loc_from start st }
  | Lexer.TRUE ->
    ignore (advance st);
    Ast.Literal (Bool true)
  | Lexer.FALSE ->
    ignore (advance st);
    Ast.Literal (Bool false)
  | Lexer.NULL ->
    ignore (advance st);
    Ast.Literal Null
  | Lexer.LBRACE -> parse_object st
  | Lexer.LBRACKET -> parse_array_construct st
  | Lexer.LPAREN -> parse_paren st
  | Lexer.IF -> parse_if st
  | Lexer.LET -> parse_let st
  | Lexer.IDENT name -> parse_fn_call name st
  | Lexer.BANG -> parse_unary st
  | _ ->
    Error.raise_
      ~loc:{ start_pos = t.pos; end_pos = t.pos + 1 }
      Parse_error
      (Printf.sprintf "unexpected token: %s" (Lexer.token_to_string t.token))

and parse_fn_call name st =
  let start = current_pos st in
  ignore (advance st);
  (* Check for lambda: name => body *)
  if peek st = Lexer.FAT_ARROW then (
    ignore (advance st);
    let body = parse_expr st in
    Ast.Lambda { param = name; body; loc = loc_from start st })
  else
    (* Collect arguments: expressions that follow the function name
       until we hit a pipe, comma, rparen, rbracket, rbrace, eof, or another
       low-precedence token *)
    let args = parse_fn_args st in
    Ast.FnCall { name; args; loc = loc_from start st }

and parse_fn_args st =
  let args = ref [] in
  let rec collect () =
    match peek st with
    | Lexer.PIPE | Lexer.COMMA | Lexer.RPAREN | Lexer.RBRACKET | Lexer.RBRACE
    | Lexer.SEMICOLON | Lexer.EOF | Lexer.THEN | Lexer.ELSE | Lexer.IN ->
      ()
    | _ ->
      let arg = parse_fn_arg st in
      args := arg :: !args;
      if peek st = Lexer.SEMICOLON then (
        ignore (advance st);
        collect ())
  in
  collect ();
  List.rev !args

and parse_fn_arg st =
  (* A function argument is a single expression at comparison level or below,
     not a full pipe expression *)
  parse_or st

and parse_object st =
  let start = current_pos st in
  expect st Lexer.LBRACE;
  let fields = ref [] in
  let rec collect () =
    match peek st with
    | Lexer.RBRACE -> ()
    | _ ->
      let field = parse_obj_field st in
      fields := field :: !fields;
      if peek st = Lexer.COMMA then (
        ignore (advance st);
        collect ())
  in
  collect ();
  expect st Lexer.RBRACE;
  Ast.ObjectConstruct { fields = List.rev !fields; loc = loc_from start st }

and parse_obj_field st =
  match peek st with
  | Lexer.IDENT name -> (
    let saved_pos = st.pos in
    ignore (advance st);
    match peek st with
    | Lexer.COLON ->
      ignore (advance st);
      let value = parse_pipe st in
      Ast.Explicit { key = name; value }
    | _ ->
      (* It's a punned field {name} => {name: .name} *)
      st.pos <- saved_pos;
      ignore (advance st);
      Ast.Punned name)
  | Lexer.STRING key ->
    ignore (advance st);
    expect st Lexer.COLON;
    let value = parse_pipe st in
    Ast.Explicit { key; value }
  | _ ->
    Error.raise_
      ~loc:{ start_pos = current_pos st; end_pos = current_pos st + 1 }
      Parse_error "expected field name in object"

and parse_array_construct st =
  let start = current_pos st in
  expect st Lexer.LBRACKET;
  let elements = ref [] in
  let rec collect () =
    match peek st with
    | Lexer.RBRACKET -> ()
    | _ ->
      let elem = parse_pipe st in
      elements := elem :: !elements;
      if peek st = Lexer.COMMA then (
        ignore (advance st);
        collect ())
  in
  collect ();
  expect st Lexer.RBRACKET;
  Ast.ArrayConstruct { elements = List.rev !elements; loc = loc_from start st }

and parse_paren st =
  expect st Lexer.LPAREN;
  let expr = parse_expr st in
  expect st Lexer.RPAREN;
  expr

and parse_if st =
  let start = current_pos st in
  expect st Lexer.IF;
  let cond = parse_expr st in
  expect st Lexer.THEN;
  let then_ = parse_expr st in
  let else_ =
    if peek st = Lexer.ELSE then (
      ignore (advance st);
      Some (parse_expr st))
    else None
  in
  Ast.If { cond; then_; else_; loc = loc_from start st }

and parse_let st =
  let start = current_pos st in
  expect st Lexer.LET;
  let name =
    match peek st with
    | Lexer.IDENT n ->
      ignore (advance st);
      n
    | _ ->
      Error.raise_
        ~loc:{ start_pos = current_pos st; end_pos = current_pos st + 1 }
        Parse_error "expected variable name after let"
  in
  (match peek st with
  | Lexer.ASSIGN -> ignore (advance st)
  | _ ->
    Error.raise_
      ~loc:{ start_pos = current_pos st; end_pos = current_pos st + 1 }
      Parse_error "expected = after variable name");
  let value = parse_pipe st in
  expect st Lexer.IN;
  let body = parse_expr st in
  Ast.Let { name; value; body; loc = loc_from start st }

let parse source =
  let st = make source in
  let expr = parse_expr st in
  if peek st <> Lexer.EOF then
    Error.raise_
      ~loc:{ start_pos = current_pos st; end_pos = current_pos st + 1 }
      Parse_error
      (Printf.sprintf "unexpected token: %s" (Lexer.token_to_string (peek st)));
  expr
