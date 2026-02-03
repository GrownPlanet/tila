open Asm

let ( let* ) = Result.bind

type dflist = Asm.t list -> Asm.t list

let empty_dlist tail = [] @ tail
let create_df init tail = init @ tail
let join_df a b tail = a (b tail)

let rec join_dfs dfs tail =
  match dfs with x :: r -> x (join_dfs r tail) | [] -> tail

type body = {
  main_function : dflist option;
  functions : dflist;
  globals : dflist;
  labels : (string, int * Ast.typ) Hashtbl.t;
  label_counter : int;
}

let next_label body name typ =
  match Hashtbl.find_opt body.labels name with
  | Some _ -> Error (Printf.sprintf "identifier '%s' already exists" name)
  | None ->
      let label = "Lb" ^ string_of_int body.label_counter in
      Hashtbl.add body.labels name (body.label_counter, typ);
      let body = { body with label_counter = body.label_counter + 1 } in
      Ok (body, label)

let next_blind_label body =
  let label = "Lb" ^ string_of_int body.label_counter in
  let body = { body with label_counter = body.label_counter + 1 } in
  Ok (body, label)

let get_label body name =
  match Hashtbl.find_opt body.labels name with
  | Some (l, t) -> Ok ("Lb" ^ string_of_int l, t)
  | None -> Error (Printf.sprintf "could not find identifier '%s'" name)

let compile_function_call body id args =
  match (id, args) with
  | "print", [ Ast.Id arg ] ->
      let* label, typ = get_label body arg in
      if typ <> Ast.TString then Error "'print' expectss tring as input"
      else Ok (body, create_df [ Ld (Hl, Id label); BCall "_PutS" ])
  | "go_home", [] -> Ok (body, create_df [ BCall "_HomeUp" ])
  | "clear_lcd", [] -> Ok (body, create_df [ BCall "_ClrLCDFull" ])
  | _, [] ->
      let* label, typ = get_label body id in
      if typ <> Ast.TVoid then
        Error (Printf.sprintf "label '%s' not callable" id)
      else Ok (body, create_df [ Call label ])
  | _ -> Error "functions with arguments aren't implemented yet"

(* compiles the literal into hl or a, depending on the input *)
let compile_expression body expression =
  match expression with
  | Ast.Literal lit -> (
      match lit with
      | LNumber n ->
          if 0 <= n && n <= 255 then
            Ok (body, create_df [ Ld (A, Lit n) ], Ast.TU8)
          else if 256 <= n && n <= 65535 then
            Ok (body, create_df [ Ld (Hl, Lit n) ], Ast.TU8)
          else Error "number out of range (16 bit)"
      | LString _ ->
          Error
            "using string literals isn't supported yet, please use a global \
             variable")
  | Ast.Id id -> (
      let* label, typ = get_label body id in
      match typ with
      | Ast.TU8 -> Ok (body, create_df [ Ld (A, IdRef label) ], typ)
      | _ -> Ok (body, create_df [ Ld (Hl, IdRef label) ], typ))
  | Ast.Binary _ ->
      Error "assigning variables to binary operations isn't supported yet"

let compile_comparison_8b left_code right_code else_label case =
  join_dfs
    [
      left_code;
      right_code;
      create_df [ Cp (A, Reg B); Jr (Some case, else_label) ];
    ]

let compile_comparison_16b left_code right_code else_label case =
  join_dfs
    [
      left_code;
      right_code;
      create_df
        [
          Ld (A, Reg H);
          Cp (A, Reg D);
          Jr (Some case, else_label);
          Ld (A, Reg L);
          Cp (A, Reg E);
          Jr (Some case, else_label);
        ];
    ]

let compile_condition body expression =
  match expression with
  | Ast.Binary { left; right; opperator } -> (
      let* body, else_label = next_blind_label body in
      let* body, left_code, ltyp = compile_expression body left in
      let* body, right_code, rtyp = compile_expression body right in
      let* left_code, right_code, compile_func =
        match (ltyp, rtyp) with
        | Ast.TU8, Ast.TU8 ->
            Ok
              ( join_df left_code (create_df [ Ld (B, Reg A) ]),
                right_code,
                compile_comparison_8b )
        | Ast.TU16, Ast.TU16 ->
            Ok
              ( join_df left_code (create_df [ Ex (De, Reg Hl) ]),
                right_code,
                compile_comparison_16b )
        | Ast.TU8, Ast.TU16 ->
            Ok
              ( join_df left_code (create_df [ Ld (D, Lit 0); Ld (E, Reg A) ]),
                right_code,
                compile_comparison_16b )
        | Ast.TU16, Ast.TU8 ->
            Ok
              ( join_df left_code (create_df [ Ex (De, Reg Hl) ]),
                join_df right_code (create_df [ Ld (H, Lit 0); Ld (L, Reg A) ]),
                compile_comparison_16b )
        | _ -> Error "invalid argument in binary expression"
      in
      match opperator with
      | Token.EqualEqual ->
          let code = compile_func left_code right_code else_label NZ in
          Ok (body, code, else_label)
      | _ -> Error "expected comparison in if statement")
  | _ -> Error "expected comparison in if statement"

let compile_if body expression then_code =
  let* body, code, else_label = compile_condition body expression in
  Ok (body, join_dfs [ code; then_code; create_df [ Label else_label ] ])

let compile_if_else body expression then_code else_code =
  let* body, code, else_label = compile_condition body expression in
  let* body, end_label = next_blind_label body in
  Ok
    ( body,
      join_dfs
        [
          code;
          then_code;
          create_df [ Jr (None, end_label); Label else_label ];
          else_code;
          create_df [ Label end_label ];
        ] )

let rec compile_statement body statement =
  match statement with
  | Ast.Block stmts -> compile_block body stmts (create_df [])
  | Ast.FunctionCall { id; args } -> compile_function_call body id args
  | Ast.If { case; then_branch; else_branch } -> (
      let* body, then_code = compile_statement body then_branch in
      match else_branch with
      | Some else_branch ->
          let* body, else_code = compile_statement body else_branch in
          compile_if_else body case then_code else_code
      | None -> compile_if body case then_code)

and compile_block body statements acc =
  match statements with
  | [] -> Ok (body, acc)
  | node :: rest ->
      let* body, code = compile_statement body node in
      compile_block body rest (join_df acc code)

let compile_func_def body id contents =
  let* body, label = next_label body id Ast.TVoid in
  let* body, contents = compile_statement body contents in
  let code =
    join_df
      (join_df (create_df [ Label label ]) contents)
      (create_df [ Return ])
  in
  if id = "main" then Ok { body with main_function = Some code }
  else Ok { body with functions = join_df body.functions code }

let split_u16 n = [ 255 land n; n lsr 8 ]

let compile_glob_assig body id value typ =
  let* body, label = next_label body id typ in
  let* value =
    match value with
    | Ast.LString s ->
        Ok (create_df [ Label label; Db [ "\"" ^ s ^ "\""; "0" ] ])
    | Ast.LNumber n -> (
        match typ with
        | Ast.TU8 ->
            if n > 255 then Error "u8 assignment out of range"
            else Ok (create_df [ Label label; Db [ string_of_int n ] ])
        | Ast.TU16 ->
            if n > 65535 then Error "u16 assignment out of range"
            else
              let bytes = List.map string_of_int (split_u16 n) in
              Ok (create_df [ Label label; Db bytes ])
        | _ -> Error "internal: impossible type for number")
  in
  Ok { body with globals = join_df body.globals value }

let compile_top_def body node =
  match node with
  | Ast.FunctionDefinition { id; contents } -> compile_func_def body id contents
  | Ast.GlobalAssignment { id; value; typ } ->
      compile_glob_assig body id value typ

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
  let body = join_dfs [ main_function; body.functions; body.globals ] in
  Ok (header (body [ End ]))
