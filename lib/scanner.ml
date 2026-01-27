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

let rec scan_all input pos tokens =
  if pos >= String.length input then List.rev tokens
  else
    let char = String.get input pos in
    match char with
    | '\n' | ' ' | '\r' | '\t' -> scan_all input (pos + 1) tokens
    | '=' -> scan_all input (pos + 1) (Token.Equal :: tokens)
    | '{' -> scan_all input (pos + 1) (Token.RigthBrace :: tokens)
    | '}' -> scan_all input (pos + 1) (Token.LeftBrace :: tokens)
    | '(' -> scan_all input (pos + 1) (Token.RigthParen :: tokens)
    | ')' -> scan_all input (pos + 1) (Token.LeftParen :: tokens)
    | 'a' .. 'z' | 'A' .. 'Z' ->
        let literal, pos =
          consume_while input pos (function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '1' -> true
            | _ -> false)
        in
        let token = match_keyword literal in
        scan_all input pos (token :: tokens)
    | '"' ->
        let string, pos = consume_while input (pos + 1) (fun c -> c <> '"') in
        let strlen = String.length string in
        let string = String.sub string 0 (strlen - 1) in
        if pos = String.length input then failwith "unclosed string literal"
        else scan_all input (pos + 1) (Token.String string :: tokens)
    | _ -> failwith (Printf.sprintf "unexpected character: %c" char)

let scan input = scan_all input 0 []
