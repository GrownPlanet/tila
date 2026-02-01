open Asm

let render_reg reg =
  match reg with
  | A -> "a"
  | H -> "h"
  | L -> "l"
  | D -> "d"
  | E -> "e"
  | Hl -> "hl"
  | De -> "de"
  | Z -> "z"
  | NZ -> "nz"

let render_value value =
  match value with
  | Reg r -> render_reg r
  | Id s -> s
  | IdRef s -> "(" ^ s ^ ")"
  | Lit n -> string_of_int n

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
  | Ld (t, f) -> "  ld " ^ render_reg t ^ ", " ^ render_value f
  | Jr (case, label) -> (
      match case with
      | Some value -> "  jr " ^ render_reg value ^ ", " ^ label
      | None -> "  jr " ^ label)
  | Cp (a, b) -> "  cp " ^ render_reg a ^ ", " ^ render_value b
  | Ex (a, b) -> "  ex " ^ render_reg a ^ ", " ^ render_value b

let emit asm out_channel =
  List.map render asm |> String.concat "\n" |> Printf.fprintf out_channel "%s\n"
