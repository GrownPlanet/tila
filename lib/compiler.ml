open Asm

let compile _ast =
  let output =
    [
      NoList;
      Include "ti83plus.inc";
      List;
      Org "userMem-2";
      Db ["t2ByteTok"; "tAsmCmp"]
    ]
    |> List.rev
  in
  let output = End :: output in
  List.rev output
