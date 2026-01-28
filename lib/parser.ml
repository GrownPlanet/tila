open Token

let ( let* ) = Result.bind

let raise_parse_error token place line =
  Error
    (Printf.sprintf "unexpected '%s' in %s on line %d" (to_string token) place
       line)

let parse_function_call_args rest =
  let rec aux rest acc =
    match rest with
    | (RightParen, _) :: r -> Ok (List.rev acc, r)
    | (TNumber num, _) :: r -> aux r (Ast.Literal (LNumber num) :: acc)
    | (TString str, _) :: r -> aux r (Ast.Literal (LString str) :: acc)
    | (Id id, _) :: r -> aux r (Ast.Id id :: acc)
    | (t, line) :: _ -> raise_parse_error t "function call" line
    | _ -> Error "unexpected end of file"
  in
  aux rest []

let rec parse_statement tokens =
  match tokens with
  | (Id id, _) :: (LeftParen, _) :: r ->
      let* args, r = parse_function_call_args r in
      Ok (Ast.FunctionCall { id; args }, r)
  | (LeftBrace, _) :: r ->
      let* statements, r = parse_block r in
      Ok (Ast.Block statements, r)
  | (t, line) :: _ -> raise_parse_error t "statement" line
  | [] -> Error "unexpected end of file"

and parse_block rest =
  let rec aux rest acc =
    match rest with
    | (RightBrace, _) :: r -> Ok (List.rev acc, r)
    | _ ->
        let* statement, rest = parse_statement rest in
        aux rest (statement :: acc)
  in
  aux rest []

let parse_function_definition rest =
  match rest with
  | (Id id, _) :: (LeftParen, _) :: (RightParen, _) :: r ->
      let* contents, r = parse_statement r in
      Ok (Ast.FunctionDefinition { id; contents }, r)
  | (_, line) :: _ ->
      Error
        (Printf.sprintf
           "invalid function definition on line %d, keep in mind input \
            variables aren't supported yet"
           line)
  | [] -> Error "unexpected end of file"

let parse_global_assignment rest =
  match rest with
  | (Id id, _) :: (Equal, _) :: (TString value, _) :: r ->
      Ok (Ast.GlobalAssignment { id; value = Ast.LString value }, r)
  | (Id id, _) :: (Equal, _) :: (TNumber value, _) :: r ->
      Ok (Ast.GlobalAssignment { id; value = Ast.LNumber value }, r)
  | (_, line) :: _ ->
      Error (Printf.sprintf "invalid global assignment on line %d" line)
  | [] -> Error "unexpected end of file"

let rec aux tokens acc =
  match tokens with
  | (Global, _) :: r ->
      let* statement, r = parse_global_assignment r in
      aux r (statement :: acc)
  | (Fn, _) :: r ->
      let* statement, r = parse_function_definition r in
      aux r (statement :: acc)
  | [] -> Ok (List.rev acc)
  | (t, line) :: _ ->
      Error
        (Printf.sprintf "unexpected token '%s' on line %d" (to_string t) line)

let parse tokens = aux tokens []
