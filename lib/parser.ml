type state = {
  tokens : Lexer.positioned_token array;
  mutable pos : int;
  source : string;
}

type assoc = Left | Right | Nonassoc

type infix_info =
  | Pipe of { precedence : int; assoc : assoc }
  | BinOp of { op : Ast.binop; precedence : int; assoc : assoc }

let make source =
  let tokens = Lexer.tokenize source |> Array.of_list in
  { tokens; pos = 0; source }

let current st =
  if st.pos < Array.length st.tokens then st.tokens.(st.pos)
  else
    {
      Lexer.token = EOF;
      start_pos = String.length st.source;
      end_pos = String.length st.source;
    }

let peek st = (current st).token

let peek_n st n =
  let idx = st.pos + n in
  if idx < Array.length st.tokens then st.tokens.(idx).token else EOF

let advance st =
  let t = current st in
  st.pos <- st.pos + 1;
  t

let expect st expected =
  let t = advance st in
  if t.token <> expected then
    Error.raise_
      ~loc:{ start_pos = t.start_pos; end_pos = t.end_pos }
      Parse_error
      (Printf.sprintf "expected %s, got %s"
         (Lexer.token_to_string expected)
         (Lexer.token_to_string t.token))

let current_pos st = (current st).start_pos

let loc_from start_pos st =
  Ast.{ start_pos; end_pos = current_pos st }

let error_at_span start_pos end_pos message =
  Error.raise_ ~loc:{ start_pos; end_pos } Parse_error message

let error_at_token st message =
  let t = current st in
  error_at_span t.start_pos t.end_pos message

let error_here st message = error_at_token st message

let previous_end_pos st =
  if st.pos > 0 then st.tokens.(st.pos - 1).end_pos else 0

let is_adjacent_to_previous st =
  previous_end_pos st = (current st).start_pos

let arg_min_bp = 11

let infix_info = function
  | Lexer.PIPE -> Some (Pipe { precedence = 10; assoc = Left })
  | Lexer.OR -> Some (BinOp { op = Or; precedence = 20; assoc = Left })
  | Lexer.AND -> Some (BinOp { op = And; precedence = 30; assoc = Left })
  | Lexer.EQ -> Some (BinOp { op = Eq; precedence = 40; assoc = Nonassoc })
  | Lexer.NEQ -> Some (BinOp { op = Neq; precedence = 40; assoc = Nonassoc })
  | Lexer.LT -> Some (BinOp { op = Lt; precedence = 50; assoc = Nonassoc })
  | Lexer.GT -> Some (BinOp { op = Gt; precedence = 50; assoc = Nonassoc })
  | Lexer.LTE -> Some (BinOp { op = Lte; precedence = 50; assoc = Nonassoc })
  | Lexer.GTE -> Some (BinOp { op = Gte; precedence = 50; assoc = Nonassoc })
  | Lexer.NULL_COALESCE ->
    Some (BinOp { op = NullCoalesce; precedence = 60; assoc = Right })
  | Lexer.CONCAT -> Some (BinOp { op = Concat; precedence = 70; assoc = Left })
  | Lexer.PLUS -> Some (BinOp { op = Add; precedence = 80; assoc = Left })
  | Lexer.MINUS -> Some (BinOp { op = Sub; precedence = 80; assoc = Left })
  | Lexer.STAR -> Some (BinOp { op = Mul; precedence = 90; assoc = Left })
  | Lexer.SLASH -> Some (BinOp { op = Div; precedence = 90; assoc = Left })
  | Lexer.PERCENT -> Some (BinOp { op = Mod; precedence = 90; assoc = Left })
  | _ -> None

let next_min_bp precedence = function
  | Left | Nonassoc -> precedence + 1
  | Right -> precedence

let is_fn_arg_terminator = function
  | Lexer.PIPE | Lexer.COMMA | Lexer.RPAREN | Lexer.RBRACKET | Lexer.RBRACE
  | Lexer.SEMICOLON | Lexer.EOF | Lexer.THEN | Lexer.ELSE | Lexer.IN ->
    true
  | _ -> false

let rec parse_expr_bp st min_bp =
  let left = parse_prefix st in
  parse_expr_tail st min_bp left None

and parse_expr_tail st min_bp left last_nonassoc =
  let left = parse_postfixes st left in
  match infix_info (peek st) with
  | Some (Pipe { precedence; assoc })
    when precedence >= min_bp
         && not (last_nonassoc = Some precedence) ->
    ignore (advance st);
    let right = parse_expr_bp st (next_min_bp precedence assoc) in
    parse_expr_tail st min_bp Ast.(Pipe { left; right }) None
  | Some (BinOp { op; precedence; assoc })
    when precedence >= min_bp
         && not (last_nonassoc = Some precedence) ->
    let op_pos = current_pos st in
    ignore (advance st);
    let right = parse_expr_bp st (next_min_bp precedence assoc) in
    let left =
      Ast.BinOp { op; left; right; loc = loc_from op_pos st }
    in
    let last_nonassoc =
      match assoc with
      | Nonassoc -> Some precedence
      | Left | Right -> None
    in
    parse_expr_tail st min_bp left last_nonassoc
  | _ -> left

and parse_postfixes st expr =
  match (peek st, is_adjacent_to_previous st) with
  | Lexer.DOT, true ->
    let dot_start = (current st).start_pos in
    let dot_end = (current st).end_pos in
    ignore (advance st);
    (match peek st with
    | Lexer.IDENT name ->
      let start = current_pos st in
      ignore (advance st);
      let optional = peek st = Lexer.QUESTION && is_adjacent_to_previous st in
      if optional then ignore (advance st);
      let field = Ast.Field { name; optional; loc = loc_from start st } in
      parse_postfixes st Ast.(Pipe { left = expr; right = field })
    | Lexer.LBRACKET when is_adjacent_to_previous st ->
      let index = parse_index st in
      parse_postfixes st Ast.(Pipe { left = expr; right = index })
    | _ -> error_at_span dot_start dot_end "expected field name or [ after .")
  | Lexer.LBRACKET, true ->
    let index = parse_index st in
    parse_postfixes st Ast.(Pipe { left = expr; right = index })
  | Lexer.QUESTION, true ->
    ignore (advance st);
    parse_postfixes st expr
  | _ -> expr

and parse_prefix st =
  let t = current st in
  match t.token with
  | Lexer.BANG ->
    ignore (advance st);
    let expr = parse_expr_bp st 95 in
    Ast.UnaryOp { op = Not; expr; loc = loc_from t.start_pos st }
  | Lexer.MINUS ->
    ignore (advance st);
    let expr = parse_expr_bp st 95 in
    Ast.BinOp
      {
        op = Sub;
        left = Ast.Literal (Int 0);
        right = expr;
        loc = loc_from t.start_pos st;
      }
  | Lexer.DOT ->
    ignore (advance st);
    (match peek st with
    | Lexer.IDENT name when is_adjacent_to_previous st ->
      let start = current_pos st in
      ignore (advance st);
      let optional = peek st = Lexer.QUESTION && is_adjacent_to_previous st in
      if optional then ignore (advance st);
      Ast.Field { name; optional; loc = loc_from start st }
    | Lexer.LBRACKET when is_adjacent_to_previous st -> parse_index st
    | _ -> Ast.Identity)
  | Lexer.INT i ->
    ignore (advance st);
    Ast.Literal (Int i)
  | Lexer.BIGINT z ->
    ignore (advance st);
    Ast.Literal (BigInt z)
  | Lexer.FLOAT f ->
    ignore (advance st);
    Ast.Literal (Float f)
  | Lexer.STRING s ->
    ignore (advance st);
    Ast.Literal (String s)
  | Lexer.INTERP_STRING parts ->
    ignore (advance st);
    let ast_parts =
      List.map
        (fun (part : Lexer.interp_part) ->
          match part with
          | Lit s -> Ast.LitPart s
          | Expr src -> Ast.ExprPart (parse src))
        parts
    in
    Ast.StringInterp { parts = ast_parts; loc = loc_from t.start_pos st }
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
  | Lexer.IDENT name -> parse_ident_expr st name t.start_pos
  | _ ->
    Error.raise_
      ~loc:{ start_pos = t.start_pos; end_pos = t.end_pos }
      Parse_error
      (Printf.sprintf "unexpected token: %s" (Lexer.token_to_string t.token))

and parse_ident_expr st name start_pos =
  ignore (advance st);
  if peek st = Lexer.FAT_ARROW then (
    ignore (advance st);
    let body = parse_expr_bp st 0 in
    Ast.Lambda { param = name; body; loc = loc_from start_pos st })
  else
    let args = parse_fn_args st in
    Ast.FnCall { name; args; loc = loc_from start_pos st }

and parse_fn_args st =
  let rec collect acc =
    match peek st with
    | token when is_fn_arg_terminator token -> List.rev acc
    | _ ->
      let arg = parse_expr_bp st arg_min_bp in
      let acc = arg :: acc in
      if peek st = Lexer.SEMICOLON then ignore (advance st);
      collect acc
  in
  collect []

and parse_index st =
  let start = current_pos st in
  expect st Lexer.LBRACKET;
  match peek st with
  | Lexer.COLON ->
    ignore (advance st);
    let end_expr =
      if peek st = Lexer.RBRACKET then None
      else Some (parse_expr_bp st 0)
    in
    expect st Lexer.RBRACKET;
    Ast.Index
      { expr = Identity; index = Slice (None, end_expr); loc = loc_from start st }
  | Lexer.RBRACKET ->
    ignore (advance st);
    Ast.Index
      { expr = Identity; index = Slice (None, None); loc = loc_from start st }
  | _ ->
    let idx = parse_expr_bp st 0 in
    if peek st = Lexer.COLON then (
      ignore (advance st);
      let end_expr =
        if peek st = Lexer.RBRACKET then None
        else Some (parse_expr_bp st 0)
      in
      expect st Lexer.RBRACKET;
      Ast.Index
        {
          expr = Identity;
          index = Slice (Some idx, end_expr);
          loc = loc_from start st;
        })
    else (
      expect st Lexer.RBRACKET;
      Ast.Index { expr = Identity; index = Single idx; loc = loc_from start st })

and parse_object st =
  let start = current_pos st in
  expect st Lexer.LBRACE;
  let rec collect acc =
    match peek st with
    | Lexer.RBRACE -> List.rev acc
    | _ ->
      let field = parse_obj_field st in
      let acc = field :: acc in
      if peek st = Lexer.COMMA then ignore (advance st);
      collect acc
  in
  let fields = collect [] in
  expect st Lexer.RBRACE;
  Ast.ObjectConstruct { fields; loc = loc_from start st }

and parse_obj_field st =
  match peek st with
  | Lexer.IDENT name -> (
    match peek_n st 1 with
    | Lexer.COLON ->
      ignore (advance st);
      ignore (advance st);
      let value = parse_expr_bp st 0 in
      Ast.Explicit { key = name; value }
    | _ ->
      ignore (advance st);
      Ast.Punned name)
  | Lexer.STRING key ->
    ignore (advance st);
    expect st Lexer.COLON;
    let value = parse_expr_bp st 0 in
    Ast.Explicit { key; value }
  | _ -> error_here st "expected field name in object"

and parse_array_construct st =
  let start = current_pos st in
  expect st Lexer.LBRACKET;
  let rec collect acc =
    match peek st with
    | Lexer.RBRACKET -> List.rev acc
    | _ ->
      let elem = parse_expr_bp st 0 in
      let acc = elem :: acc in
      if peek st = Lexer.COMMA then ignore (advance st);
      collect acc
  in
  let elements = collect [] in
  expect st Lexer.RBRACKET;
  Ast.ArrayConstruct { elements; loc = loc_from start st }

and parse_paren st =
  expect st Lexer.LPAREN;
  let expr = parse_expr_bp st 0 in
  expect st Lexer.RPAREN;
  expr

and parse_if st =
  let start = current_pos st in
  expect st Lexer.IF;
  let cond = parse_expr_bp st 0 in
  expect st Lexer.THEN;
  let then_ = parse_expr_bp st 0 in
  let else_ =
    if peek st = Lexer.ELSE then (
      ignore (advance st);
      Some (parse_expr_bp st 0))
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
    | _ -> error_here st "expected variable name after let"
  in
  (match peek st with
  | Lexer.ASSIGN -> ignore (advance st)
  | _ -> error_here st "expected = after variable name");
  let value = parse_expr_bp st 0 in
  expect st Lexer.IN;
  let body = parse_expr_bp st 0 in
  Ast.Let { name; value; body; loc = loc_from start st }

and parse source =
  let st = make source in
  let expr = parse_expr_bp st 0 in
  if peek st <> Lexer.EOF then
    Error.raise_
      ~loc:{ start_pos = (current st).start_pos; end_pos = (current st).end_pos }
      Parse_error
      (Printf.sprintf "unexpected token: %s" (Lexer.token_to_string (peek st)));
  expr
