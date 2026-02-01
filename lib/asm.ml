type reg =
  | A
  | H
  | L
  | D
  | E
  | Hl
  | De
  | Z
  | NZ

type value =
  | Reg of reg
  | Id of string
  | IdRef of string
  | Lit of int

type t =
  | NoList
  | List
  | End
  | Return
  | Include of string
  | Org of string
  | Label of string
  | Call of string
  | BCall of string
  | Db of string list
  | Jr of reg option * string
  | Ld of reg * value
  | Cp of reg * value
  | Ex of reg * value
