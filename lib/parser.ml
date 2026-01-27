let parse_function_call_args rest =
  let rec aux rest acc =
    match rest with
    | Token.RightParen :: r -> (List.rev acc, r)
    | Token.TNumber num :: r -> aux r Ast.(Literal (LNumber num) :: acc)
    | Token.TString str :: r -> aux r Ast.(Literal (LString str) :: acc)
    | Token.Id id :: r -> aux r (Ast.Id id :: acc)
    | _ -> failwith "unexpected token in function call"
  in
  aux rest []

let rec parse_statement tokens =
  match tokens with
  | Token.Id id :: Token.LeftParen :: r ->
      let args, r = parse_function_call_args r in
      (Ast.FunctionCall { id; args }, r)
  | Token.LeftBrace :: r ->
      let statements, r = parse_block r in
      (Ast.Block statements, r)
  | Token.Global :: _ -> failwith "illegal global declaration"
  | Token.Fn :: _ -> failwith "illegal function declaration"
  | t :: _ -> failwith ("unexpected token: " ^ Token.to_string t)
  | [] -> failwith "unexpected end of file"

and parse_block rest =
  let rec aux rest acc =
    match rest with
    | Token.RightBrace :: r -> (List.rev acc, r)
    | _ ->
        let statement, rest = parse_statement rest in
        aux rest (statement :: acc)
  in
  aux rest []

let parse_function_definition rest =
  match rest with
  | Token.Id id :: Token.LeftParen :: Token.RightParen :: r ->
      let content, r = parse_statement r in
      (Ast.FunctionDefenition { id; content }, r)
  | _ ->
      failwith
        "invalid function deffinition, keep in mind input variables aren't \
         supported yet"

let parse_global_assignment rest =
  match rest with
  | Token.Id id :: Token.Equal :: Token.TString value :: r ->
      (Ast.GlobalAssignment { id; value = Ast.LString value }, r)
  | Token.Id id :: Token.Equal :: Token.TNumber value :: r ->
      (Ast.GlobalAssignment { id; value = Ast.LNumber value }, r)
  | _ -> failwith "invalid global assignment"

let rec aux tokens acc =
  match tokens with
  | Token.Global :: r ->
      let statement, r = parse_global_assignment r in
      aux r (statement :: acc)
  | Token.Fn :: r ->
      let statement, r = parse_function_definition r in
      aux r (statement :: acc)
  | [] -> List.rev acc
  | t :: _ -> failwith ("unexpected token: " ^ Token.to_string t)

let parse tokens = aux tokens []
