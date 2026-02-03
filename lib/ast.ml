type typ = TU8 | TU16 | TString | TVoid
type literal = LNumber of int | LString of string

type expression =
  | Literal of literal
  | Id of string
  | Binary of { left : expression; right : expression; opperator : Token.t }

type statement =
  | FunctionCall of { id : string; args : expression list }
  | Block of statement list
  | If of {
      case : expression;
      then_branch : statement;
      else_branch : statement option;
    }

type top_level_definition =
  | FunctionDefinition of { id : string; contents : statement }
  | GlobalAssignment of { id : string; value : literal; typ : typ }

(***** debug functions *****)
let literal_to_string literal =
  match literal with
  | LNumber i -> string_of_int i
  | LString s -> Printf.sprintf "\"%s\"" s

let typ_to_str typ =
  match typ with
  | TU8 -> "u8"
  | TU16 -> "u16"
  | TString -> "string"
  | TVoid -> "void"

let rec expression_to_string expression =
  match expression with
  | Literal lit -> Printf.sprintf "(Lit: %s)" (literal_to_string lit)
  | Id s -> Printf.sprintf "(Id: %s)" s
  | Binary { left; right; opperator } ->
      Printf.sprintf "(%s %s %s)"
        (Token.to_string opperator)
        (expression_to_string left)
        (expression_to_string right)

let rec statement_to_string statement =
  match statement with
  | FunctionCall { id; args } ->
      let args_str =
        List.fold_left (fun acc e -> acc ^ expression_to_string e) "" args
      in
      Printf.sprintf "(FunctionCall: %s [%s])" id args_str
  | Block statements ->
      let statements_str =
        List.fold_left
          (fun acc s -> acc ^ " " ^ statement_to_string s)
          "" statements
      in
      Printf.sprintf "(Block:%s)" statements_str
  | If { case; then_branch; else_branch } ->
      let case_str = expression_to_string case in
      let then_str = statement_to_string then_branch in
      let else_str =
        match else_branch with
        | None -> "None"
        | Some s -> statement_to_string s
      in
      Printf.sprintf "(If %s then %s else %s)" case_str then_str else_str

let print_top_level_declaration decl =
  match decl with
  | FunctionDefinition { id; contents } ->
      let contents_str = statement_to_string contents in
      Printf.sprintf "(FunctionDefinition: %s %s)" id contents_str
  | GlobalAssignment { id; value; typ } ->
      let value_str = literal_to_string value in
      let typ_str = typ_to_str typ in
      Printf.sprintf "(GlobalAssignment: %s %s = %s)" typ_str id value_str
