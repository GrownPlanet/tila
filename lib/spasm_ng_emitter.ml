open Asm

let render instruction =
  match instruction with
  | NoList -> ".nolist"
  | List -> ".list"
  | Include file -> Printf.sprintf "#include \"%s\"" file
  | Org s -> ".org " ^ s
  | Db data -> ".db " ^ (String.concat ", " data)
  | End -> ".end"

let emit asm out_channel =
  List.map render asm
  |> String.concat "\n"
  |> Printf.fprintf out_channel "%s\n"
