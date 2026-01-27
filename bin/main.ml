open Tilang

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "usage: %s [program]\n" Sys.argv.(0)
  else
    let input = In_channel.(with_open_text Sys.argv.(1) input_all) in
    let tokens = Lexer.lex input in
    match Parser.parse tokens with
    | Ok ast ->
        List.iter (fun s -> Ast.statement_to_string s |> print_endline) ast
    | Error e -> Printf.printf "error: %s\n" e
