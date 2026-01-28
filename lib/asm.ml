type reg = Hl | Id of string

type t =
  | NoList
  | List
  | Include of string
  | Org of string
  | Db of string list
  | End
  | Label of string
  | Call of string
  | BCall of string
  | Return
  | Ld of reg * reg
