open Asm

let ( let* ) = Result.bind

type dflist = Asm.t list -> Asm.t list

let empty_dlist tail = [] @ tail
let create_df init tail = init @ tail
let join_df a b tail = a (b tail)

type body = {
  main_function : dflist option;
  functions : dflist;
  globals : dflist;
  labels : (string, int) Hashtbl.t;
  label_counter : int;
}

let next_label body name =
  match Hashtbl.find_opt body.labels name with
  | Some _ -> Error (Printf.sprintf "identifier '%s' already exists" name)
  | None ->
      let label = "Lb" ^ string_of_int body.label_counter in
      Hashtbl.add body.labels name body.label_counter;
      let body = { body with label_counter = body.label_counter + 1 } in
      Ok (body, label)

let get_label body name =
  match Hashtbl.find_opt body.labels name with
  | Some l -> Ok ("Lb" ^ string_of_int l)
  | None -> Error (Printf.sprintf "could not find identifier '%s'" name)

let compile_function_call body id args =
  match (id, args) with
  | "print", [ Ast.Id id ] ->
      let* label = get_label body id in
      Ok (body, create_df [ Ld (Hl, Id label); BCall "_PutS" ])
  | "goHome", [] -> Ok (body, create_df [ BCall "_HomeUp" ])
  | "clearLCD", [] -> Ok (body, create_df [ BCall "_ClrLCDFull" ])
  | _ -> failwith "calling regular functions: not implemented yet"

let rec compile_statement body statement =
  match statement with
  | Ast.Block stmts -> compile_block body stmts (create_df [])
  | Ast.FunctionCall { id; args } -> compile_function_call body id args

and compile_block body statements acc =
  match statements with
  | [] -> Ok (body, acc)
  | node :: rest ->
      let* body, code = compile_statement body node in
      compile_block body rest (join_df acc code)

let compile_func_def body id contents =
  let* body, label = next_label body id in
  let* body, contents = compile_statement body contents in
  let code =
    join_df
      (join_df (create_df [ Label label ]) contents)
      (create_df [ Return ])
  in
  if id = "main" then Ok { body with main_function = Some code }
  else Ok { body with functions = join_df body.functions code }

let compile_glob_assig body id value =
  let* body, label = next_label body id in
  let value =
    match value with
    | Ast.LNumber n -> create_df [ Label label; Db [ string_of_int n ] ]
    | Ast.LString s -> create_df [ Label label; Db [ "\"" ^ s ^ "\""; "0" ] ]
  in
  Ok { body with globals = join_df body.globals value }

let compile_top_def body node =
  match node with
  | Ast.FunctionDefinition { id; contents } -> compile_func_def body id contents
  | Ast.GlobalAssignment { id; value } -> compile_glob_assig body id value

let rec compile_all ast body =
  match ast with
  | [] -> Ok body
  | node :: rest -> (
      match compile_top_def body node with
      | Ok body -> compile_all rest body
      | Error e -> Error e)

let compile ast =
  let header =
    create_df
      [
        NoList;
        Include "ti83plus.inc";
        List;
        Org "userMem-2";
        Db [ "t2ByteTok"; "tAsmCmp" ];
      ]
  in
  let empty_body =
    {
      main_function = None;
      functions = empty_dlist;
      globals = empty_dlist;
      labels = Hashtbl.create 16;
      label_counter = 0;
    }
  in
  let* body = compile_all ast empty_body in
  let* main_function =
    match body.main_function with
    | Some f -> Ok f
    | None -> Error "no 'main' function"
  in
  let body = body.globals |> join_df body.functions |> join_df main_function in
  Ok (header (body [ End ]))
