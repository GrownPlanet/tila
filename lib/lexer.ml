let consume_while input pos pred =
  let len = String.length input in
  let rec aux i acc =
    if i < len && pred (String.get input i) then
      aux (i + 1) (acc ^ String.make 1 (String.get input i))
    else (acc, i)
  in
  aux pos ""

let match_keyword literal =
  match literal with
  | "fn" -> Token.Fn
  | "global" -> Token.Global
  | _ -> Token.Id literal

let rec lex_all input pos tokens =
  if pos >= String.length input then List.rev tokens
  else
    let char = String.get input pos in
    match char with
    | '\n' | ' ' | '\r' | '\t' -> lex_all input (pos + 1) tokens
    | '=' -> lex_all input (pos + 1) (Token.Equal :: tokens)
    | '{' -> lex_all input (pos + 1) (Token.LeftBrace :: tokens)
    | '}' -> lex_all input (pos + 1) (Token.RightBrace :: tokens)
    | '(' -> lex_all input (pos + 1) (Token.LeftParen :: tokens)
    | ')' -> lex_all input (pos + 1) (Token.RightParen :: tokens)
    | 'a' .. 'z' | 'A' .. 'Z' ->
        let literal, pos =
          consume_while input pos (function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '1' -> true
            | _ -> false)
        in
        let token = match_keyword literal in
        lex_all input pos (token :: tokens)
    | '"' ->
        let string, pos = consume_while input (pos + 1) (fun c -> c <> '"') in
        let strlen = String.length string in
        let string = String.sub string 0 (strlen - 1) in
        if pos = String.length input then failwith "unclosed string literal"
        else lex_all input (pos + 1) (Token.TString string :: tokens)
    | _ -> failwith (Printf.sprintf "unexpected character: %c" char)

let lex input = lex_all input 0 []
