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
  | (Token.Id id, _) :: (Token.LeftParen, _) :: r -> (
      match parse_function_call_args r with
      | Ok (args, r) -> Ok (Ast.FunctionCall { id; args }, r)
      | Error e -> Error e)
  | (Token.LeftBrace, _) :: r -> (
      match parse_block r with
      | Ok (statements, r) -> Ok (Ast.Block statements, r)
      | Error e -> Error e)
  | (t, line) :: _ -> raise_parse_error t "statement" line
  | [] -> Error "unexpected end of file"

and parse_block rest =
  let rec aux rest acc =
    match rest with
    | (Token.RightBrace, _) :: r -> Ok (List.rev acc, r)
    | _ -> (
        match parse_statement rest with
        | Ok (statement, rest) -> aux rest (statement :: acc)
        | Error e -> Error e)
  in
  aux rest []

let parse_function_definition rest =
  match rest with
  | (Token.Id id, _) :: (Token.LeftParen, _) :: (Token.RightParen, _) :: r -> (
      match parse_statement r with
      | Ok (content, r) -> Ok (Ast.FunctionDefinition { id; content }, r)
      | Error e -> Error e)
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
  | (Token.Global, _) :: r -> (
      match parse_global_assignment r with
      | Ok (statement, r) -> aux r (statement :: acc)
      | Error e -> Error e)
  | (Token.Fn, _) :: r -> (
      match parse_function_definition r with
      | Ok (statement, r) -> aux r (statement :: acc)
      | Error e -> Error e)
  | [] -> Ok (List.rev acc)
  | (t, line) :: _ ->
      Error
        (Printf.sprintf "unexpected token '%s' on line %d" (Token.to_string t)
           line)

let parse tokens = aux tokens []
