open Tilang

let compile filename =
  let ( let* ) = Result.bind in
  let input = In_channel.(with_open_text filename input_all) in
  let* tokens = Lexer.lex input in
  let* ast = Parser.parse tokens in
  Ok ast

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "usage: %s [program]\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    match compile filename with
    | Error e -> Printf.eprintf "error: %s\n" e
    | Ok ast -> List.iter (fun s -> Ast.statement_to_string s |> print_endline) ast
