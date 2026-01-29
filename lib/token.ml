type t =
  (* Keywords *)
  | Fn
  | Global
  | If
  | Else
  (* literals *)
  | TNumber of int
  | TString of string
  | Id of string
  (* sepperators *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  (* opperators *)
  | Plus
  | Minus
  | Equal
  | EqualEqual

let to_string token =
  match token with
  | Fn -> "Fn"
  | Global -> "Global"
  | If -> "If"
  | Else -> "Else"
  | TNumber n -> Printf.sprintf "Number %d" n
  | TString s -> Printf.sprintf "String %s" s
  | Id i -> Printf.sprintf "Id %s" i
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Equal -> "Equal"
  | EqualEqual -> "EqualEqual"
