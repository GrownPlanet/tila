open Tilang

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "usage: %s [program]\n" Sys.argv.(0)
  else
    try
      let input = In_channel.(with_open_text Sys.argv.(1) input_all) in
      let tokens = Scanner.scan input in
      List.iter Token.print tokens
    with Failure s -> print_endline s
