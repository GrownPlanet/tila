open Tila

let compile in_filename out_filename =
  let ( let* ) = Result.bind in
  let input = In_channel.(with_open_text in_filename input_all) in
  let* tokens = Lexer.lex input in
  (* List.iter (fun (t, _) -> Token.to_string t |> print_endline) tokens; *)
  let* ast = Parser.parse tokens in
  (* List.iter (fun s -> Ast.statement_to_string s |> print_endline) ast; *)
  let* asm = Compiler.compile ast in
  Ok (Out_channel.with_open_text out_filename (Spasm_ng_emitter.emit asm))

let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "usage: %s [program] [output]\n" Sys.argv.(0)
  else
    let in_filename = Sys.argv.(1) in
    let out_filename = Sys.argv.(2) in
    match compile in_filename out_filename with
    | Ok () -> Printf.printf "Done compiling. The code lives.\n"
    | Error e -> Printf.eprintf "error: %s\n" e
