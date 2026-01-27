type t =
  (* Keywords *)
  | Fn
  | Global
  (* literals *)
  | Number of int
  | String of string
  | Id of string
  (* sepperators *)
  | LeftParen
  | RigthParen
  | LeftBrace
  | RigthBrace
  (* opperators *)
  | Plus
  | Minus
  | Equal

let print token =
  (match token with
  | Fn -> "Fn"
  | Global -> "Global"
  | Number n -> Printf.sprintf "Number %d" n
  | String s -> Printf.sprintf "String %s" s
  | Id i -> Printf.sprintf "Id %s" i
  | LeftParen -> "LeftParen"
  | RigthParen -> "RigthParen"
  | LeftBrace -> "LeftBrace"
  | RigthBrace -> "RigthBrace"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Equal -> "Equal")
  |> print_endline
