type t =
  (* Keywords *)
  | Fn
  | Global
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

let to_string token =
  match token with
  | Fn -> "Fn"
  | Global -> "Global"
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
