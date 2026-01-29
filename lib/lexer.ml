let consume_while input pos pred =
  let len = String.length input in
  let rec aux i acc =
    if i < len && pred (String.get input i) then
      aux (i + 1) (acc ^ String.make 1 (String.get input i))
    else (acc, i)
  in
  aux pos ""

let peek input pos =
  if pos + 1 >= String.length input then None
  else Some (pos, String.get input (pos + 1))

let match_keyword literal =
  match literal with
  | "fn" -> Token.Fn
  | "global" -> Token.Global
  | _ -> Token.Id literal

let rec lex_all input pos line tokens =
  if pos >= String.length input then Ok (List.rev tokens)
  else
    let char = String.get input pos in
    match char with
    | '\n' -> lex_all input (pos + 1) (line + 1) tokens
    | ' ' | '\r' | '\t' -> lex_all input (pos + 1) line tokens
    | '=' -> lex_all input (pos + 1) line ((Token.Equal, line) :: tokens)
    | '{' -> lex_all input (pos + 1) line ((Token.LeftBrace, line) :: tokens)
    | '}' -> lex_all input (pos + 1) line ((Token.RightBrace, line) :: tokens)
    | '(' -> lex_all input (pos + 1) line ((Token.LeftParen, line) :: tokens)
    | ')' -> lex_all input (pos + 1) line ((Token.RightParen, line) :: tokens)
    | '/' -> (
        match peek input pos with
        | Some (pos, '/') ->
            let _, pos = consume_while input (pos + 1) (fun c -> c <> '\n') in
            lex_all input (pos + 1) (line + 1) tokens
        | _ -> Error "expected '/' after '/'")
    | 'a' .. 'z' | 'A' .. 'Z' ->
        let literal, pos =
          consume_while input pos (function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
            | _ -> false)
        in
        let token = match_keyword literal in
        lex_all input pos line ((token, line) :: tokens)
    | '0' .. '9' ->
        let literal, pos =
          consume_while input pos (function '0' .. '9' -> true | _ -> false)
        in
        let literal = literal |> int_of_string in
        lex_all input pos line ((Token.TNumber literal, line) :: tokens)
    | '"' ->
        let string, pos =
          consume_while input (pos + 1) (fun c -> c <> '"' && c <> '\n')
        in
        if pos = String.length input then
          Error (Printf.sprintf "unclosed string literal on line %d" line)
        else
          lex_all input (pos + 1) line ((Token.TString string, line) :: tokens)
    | _ ->
        Error (Printf.sprintf "unexpected character '%c' on line %d" char line)

let lex input = lex_all input 0 1 []
