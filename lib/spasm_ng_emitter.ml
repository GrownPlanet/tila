open Asm

let render_reg reg = match reg with Hl -> "hl" | Id s -> s

let render instruction =
  match instruction with
  | NoList -> ".nolist"
  | List -> ".list"
  | Include file -> Printf.sprintf "#include \"%s\"" file
  | Org s -> ".org " ^ s
  | Db data -> ".db " ^ String.concat ", " data
  | End -> ".end"
  | Return -> "  ret"
  | Label l -> l ^ ":"
  | Call c -> "  call " ^ c
  | BCall c -> "  b_call " ^ c
  | Ld (t, f) -> "  ld " ^ render_reg t ^ ", " ^ render_reg f

let emit asm out_channel =
  List.map render asm |> String.concat "\n" |> Printf.fprintf out_channel "%s\n"
