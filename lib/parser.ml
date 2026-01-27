let ( let* ) = Result.bind

let raise_parse_error token place line =
  Error
    (Printf.sprintf "unexpected '%s' in %s on line %d" (Token.to_string token)
       place line)

let parse_function_call_args rest =
  let rec aux rest acc =
    match rest with
    | (Token.RightParen, _) :: r -> Ok (List.rev acc, r)
    | (Token.TNumber num, _) :: r -> aux r (Ast.Literal (LNumber num) :: acc)
    | (Token.TString str, _) :: r -> aux r (Ast.Literal (LString str) :: acc)
    | (Token.Id id, _) :: r -> aux r (Ast.Id id :: acc)
    | (t, line) :: _ -> raise_parse_error t "function call" line
    | _ -> Error "unexpected end of file"
  in
  aux rest []

let rec parse_statement tokens =
  match tokens with
  | (Token.Id id, _) :: (Token.LeftParen, _) :: r ->
      let* args, r = parse_function_call_args r in
      Ok (Ast.FunctionCall { id; args }, r)
  | (Token.LeftBrace, _) :: r ->
      let* statements, r = parse_block r in
      Ok (Ast.Block statements, r)
  | (t, line) :: _ -> raise_parse_error t "statement" line
  | [] -> Error "unexpected end of file"

and parse_block rest =
  let rec aux rest acc =
    match rest with
    | (Token.RightBrace, _) :: r -> Ok (List.rev acc, r)
    | _ -> 
        let* statement, rest = parse_statement rest in
        aux rest (statement :: acc)
  in
  aux rest []

let parse_function_definition rest =
  match rest with
  | (Token.Id id, _) :: (Token.LeftParen, _) :: (Token.RightParen, _) :: r ->
      let* content, r = parse_statement r in
      Ok (Ast.FunctionDefinition { id; content }, r)
  | (_, line) :: _ ->
      Error
        (Printf.sprintf
           "invalid function deffinition on line %d, keep in mind input \
            variables aren't supported yet"
           line)
  | [] -> Error "unexpected end of file"

let parse_global_assignment rest =
  match rest with
  | (Token.Id id, _) :: (Token.Equal, _) :: (Token.TString value, _) :: r ->
      Ok (Ast.GlobalAssignment { id; value = Ast.LString value }, r)
  | (Token.Id id, _) :: (Token.Equal, _) :: (Token.TNumber value, _) :: r ->
      Ok (Ast.GlobalAssignment { id; value = Ast.LNumber value }, r)
  | (_, line) :: _ ->
      Error (Printf.sprintf "invalid global assignment on line %d" line)
  | [] -> Error "unexpected end of file"

let rec aux tokens acc =
  match tokens with
  | (Token.Global, _) :: r ->
      let* statement, r = parse_global_assignment r in
      aux r (statement :: acc)
  | (Token.Fn, _) :: r ->
      let* statement, r = parse_function_definition r in
      aux r (statement :: acc)
  | [] -> Ok (List.rev acc)
  | (t, line) :: _ ->
      Error
        (Printf.sprintf "unexpected token '%s' on line %d" (Token.to_string t)
           line)

let parse tokens = aux tokens []
