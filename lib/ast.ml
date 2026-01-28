type literal = LNumber of int | LString of string
type expression = Literal of literal | Id of string

type statement =
  | FunctionCall of { id : string; args : expression list }
  | Block of statement list

type top_level_definition =
  | FunctionDefinition of { id : string; contents : statement }
  | GlobalAssignment of { id : string; value : literal }

let literal_to_string literal =
  match literal with
  | LNumber i -> string_of_int i
  | LString s -> Printf.sprintf "\"%s\"" s

let expression_to_string expression =
  match expression with
  | Literal lit -> Printf.sprintf "{ Lit: %s }" (literal_to_string lit)
  | Id s -> Printf.sprintf "{ Id: %s }" s

let rec statement_to_string statement =
  match statement with
  | FunctionCall { id; args } ->
      let args_str =
        List.fold_left (fun acc e -> acc ^ expression_to_string e) "" args
      in
      Printf.sprintf "{ FunctionCall: %s %s }" id args_str
  | Block statements ->
      let statements_str =
        List.fold_left (fun acc s -> acc ^ statement_to_string s) "" statements
      in
      Printf.sprintf "{ Block: %s }" statements_str

let print_top_level_declaration decl =
  match decl with
  | FunctionDefinition { id; contents } ->
      let contents_str = statement_to_string contents in
      Printf.sprintf "{ FunctionDefinition: %s %s }" id contents_str
  | GlobalAssignment { id; value } ->
      let value_str = literal_to_string value in
      Printf.sprintf "{ GlobalAssignment: %s %s }" id value_str
