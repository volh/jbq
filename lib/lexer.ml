type interp_part = Lit of string | Expr of string

type token =
  | DOT
  | PIPE
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | COLON
  | COMMA
  | SEMICOLON
  | QUESTION
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | CONCAT (* ++ *)
  | NULL_COALESCE (* ?? *)
  | EQ (* == *)
  | NEQ (* != *)
  | LT
  | GT
  | LTE (* <= *)
  | GTE (* >= *)
  | AND (* && *)
  | OR (* || *)
  | BANG (* ! *)
  | ASSIGN (* = *)
  | FAT_ARROW (* => *)
  | INT of int
  | FLOAT of float
  | STRING of string
  | INTERP_STRING of interp_part list
  | IDENT of string
  | TRUE
  | FALSE
  | NULL
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | EOF

let token_to_string = function
  | DOT -> "."
  | PIPE -> "|"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LPAREN -> "("
  | RPAREN -> ")"
  | COLON -> ":"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | QUESTION -> "?"
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | PERCENT -> "%"
  | CONCAT -> "++"
  | NULL_COALESCE -> "??"
  | EQ -> "=="
  | NEQ -> "!="
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | AND -> "&&"
  | OR -> "||"
  | BANG -> "!"
  | ASSIGN -> "="
  | FAT_ARROW -> "=>"
  | INT i -> string_of_int i
  | FLOAT f -> string_of_float f
  | STRING s -> Printf.sprintf "%S" s
  | INTERP_STRING _ -> "<interp_string>"
  | IDENT s -> s
  | TRUE -> "true"
  | FALSE -> "false"
  | NULL -> "null"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | LET -> "let"
  | IN -> "in"
  | EOF -> "<eof>"

type positioned_token = {
  token : token;
  start_pos : int;
  end_pos : int;
}

let keywords =
  [
    ("true", TRUE);
    ("false", FALSE);
    ("null", NULL);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("let", LET);
    ("in", IN);
  ]

let tokenize (input : string) : positioned_token list =
  let buf = Sedlexing.Utf8.from_string input in
  let tokens = ref [] in
  let span () = Sedlexing.loc buf in
  let add token =
    let start_pos, end_pos = span () in
    tokens := { token; start_pos; end_pos } :: !tokens
  in
  let rec scan () =
    match%sedlex buf with
    | Plus (Chars " \t\n\r") -> scan ()
    | "#" ->
      skip_comment ();
      scan ()
    | "." -> add DOT; scan ()
    | "|" -> add PIPE; scan ()
    | "[" -> add LBRACKET; scan ()
    | "]" -> add RBRACKET; scan ()
    | "{" -> add LBRACE; scan ()
    | "}" -> add RBRACE; scan ()
    | "(" -> add LPAREN; scan ()
    | ")" -> add RPAREN; scan ()
    | ":" -> add COLON; scan ()
    | "," -> add COMMA; scan ()
    | ";" -> add SEMICOLON; scan ()
    | "++" -> add CONCAT; scan ()
    | "??" -> add NULL_COALESCE; scan ()
    | "?" -> add QUESTION; scan ()
    | "==" -> add EQ; scan ()
    | "!=" -> add NEQ; scan ()
    | "=>" -> add FAT_ARROW; scan ()
    | "<=" -> add LTE; scan ()
    | ">=" -> add GTE; scan ()
    | "&&" -> add AND; scan ()
    | "||" -> add OR; scan ()
    | "!" -> add BANG; scan ()
    | "=" -> add ASSIGN; scan ()
    | "<" -> add LT; scan ()
    | ">" -> add GT; scan ()
    | "+" -> add PLUS; scan ()
    | "-" -> add MINUS; scan ()
    | "*" -> add STAR; scan ()
    | "/" -> add SLASH; scan ()
    | "%" -> add PERCENT; scan ()
    | Plus ('0' .. '9'), ".", Plus ('0' .. '9') ->
      add (FLOAT (float_of_string (Sedlexing.Utf8.lexeme buf)));
      scan ()
    | Plus ('0' .. '9') ->
      add (INT (int_of_string (Sedlexing.Utf8.lexeme buf)));
      scan ()
    | '"' ->
      let parts = scan_interp_string (Buffer.create 64) [] in
      (match parts with
      | [ Lit s ] -> add (STRING s)
      | parts -> add (INTERP_STRING parts));
      scan ()
    | ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') ->
      let word = Sedlexing.Utf8.lexeme buf in
      let token =
        match List.assoc_opt word keywords with Some kw -> kw | None -> IDENT word
      in
      add token;
      scan ()
    | eof -> add EOF
    | _ ->
      let ch = Sedlexing.Utf8.lexeme buf in
      let start_pos, end_pos = span () in
      Error.raise_ ~loc:{ start_pos; end_pos } Parse_error
        (Printf.sprintf "unexpected character: %s" ch)
  and scan_interp_string strbuf acc =
    match%sedlex buf with
    | '"' ->
      let s = Buffer.contents strbuf in
      let acc = if String.length s > 0 then Lit s :: acc else acc in
      List.rev acc
    | "${" ->
      let s = Buffer.contents strbuf in
      let acc = if String.length s > 0 then Lit s :: acc else acc in
      let expr_str = scan_interp_expr (Buffer.create 32) 0 in
      scan_interp_string (Buffer.create 64) (Expr expr_str :: acc)
    | "\\\"" ->
      Buffer.add_char strbuf '"';
      scan_interp_string strbuf acc
    | "\\\\" ->
      Buffer.add_char strbuf '\\';
      scan_interp_string strbuf acc
    | "\\n" ->
      Buffer.add_char strbuf '\n';
      scan_interp_string strbuf acc
    | "\\t" ->
      Buffer.add_char strbuf '\t';
      scan_interp_string strbuf acc
    | "\\$" ->
      Buffer.add_char strbuf '$';
      scan_interp_string strbuf acc
    | any ->
      Buffer.add_string strbuf (Sedlexing.Utf8.lexeme buf);
      scan_interp_string strbuf acc
    | _ -> Error.raise_ Parse_error "unterminated string"
  and scan_interp_expr exprbuf depth =
    match%sedlex buf with
    | "}" ->
      if depth = 0 then Buffer.contents exprbuf
      else (
        Buffer.add_char exprbuf '}';
        scan_interp_expr exprbuf (depth - 1))
    | "{" ->
      Buffer.add_char exprbuf '{';
      scan_interp_expr exprbuf (depth + 1)
    | '"' ->
      Buffer.add_char exprbuf '"';
      scan_interp_expr_string exprbuf;
      scan_interp_expr exprbuf depth
    | any ->
      Buffer.add_string exprbuf (Sedlexing.Utf8.lexeme buf);
      scan_interp_expr exprbuf depth
    | _ -> Error.raise_ Parse_error "unterminated interpolation, expected }"
  and scan_interp_expr_string exprbuf =
    match%sedlex buf with
    | '"' -> Buffer.add_char exprbuf '"'
    | "\\\"" ->
      Buffer.add_string exprbuf "\\\"";
      scan_interp_expr_string exprbuf
    | any ->
      Buffer.add_string exprbuf (Sedlexing.Utf8.lexeme buf);
      scan_interp_expr_string exprbuf
    | _ -> Error.raise_ Parse_error "unterminated string inside interpolation"
  and skip_comment () =
    match%sedlex buf with '\n' | eof -> () | _ -> skip_comment ()
  in
  scan ();
  List.rev !tokens
