open Ppxlib

let to_encoder_name i = i ^ "_encoder"

let rec expr_of_typ (typ : core_type)
    ~(substitutions : (core_type * expression) list) : expression =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  match typ with
  | [%type: unit] | [%type: unit] -> Ast_builder.Default.evar ~loc "E.null"
  | [%type: int] -> Ast_builder.Default.evar ~loc "E.int"
  | [%type: int32]
  | [%type: Int32.t]
  | [%type: int64]
  | [%type: Int64.t]
  | [%type: nativeint]
  | [%type: Nativeint.t] ->
      failwith "Cannot yet handle any int-like but int"
  | [%type: float] -> Ast_builder.Default.evar ~loc "E.float"
  | [%type: bool] -> Ast_builder.Default.evar ~loc "E.bool"
  | [%type: char] ->
      failwith "Cannot directly handle character; please cast to string first"
  | [%type: string] | [%type: String.t] ->
      Ast_builder.Default.evar ~loc "E.string"
  | [%type: bytes] | [%type: Bytes.t] ->
      failwith "Cannot handle Bytes" (* TODO: figure out strategy *)
  | [%type: [%t? inner_typ] list] ->
      let list_decoder = Ast_builder.Default.evar ~loc "E.list" in
      let sub_expr = expr_of_typ ~substitutions inner_typ in
      Ast_helper.Exp.apply ~loc list_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] array] ->
      let array_decoder = Ast_builder.Default.evar ~loc "E.array" in
      let sub_expr = expr_of_typ ~substitutions inner_typ in
      Ast_helper.Exp.apply ~loc array_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] option] ->
      let opt_decoder = Ast_builder.Default.evar ~loc "E.nullable" in
      let sub_expr = expr_of_typ ~substitutions inner_typ in
      Ast_helper.Exp.apply ~loc opt_decoder [ (Nolabel, sub_expr) ]
  | { ptyp_desc = Ptyp_tuple _typs; _ } ->
      failwith "NYI" (* expr_of_tuple ~substitutions ~loc typs *)
  (* | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } -> _ *)
  (* | { ptyp_desc = Ptyp_alias _; _ } -> *)
  (*     failwith *)
  (*       (Format.sprintf "This alias was a failure...: %s\n" *)
  (*          (string_of_core_type typ)) *)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident lid; _ }, []); _ } as other_type
    -> (
      (* In the case where our type is truly recursive, we need to instead do `type_aux ()` *)
      let eq (ct1 : core_type) (ct2 : core_type) =
        (* TODO: This is a terrible way to compare the types... *)
        string_of_core_type ct1 = string_of_core_type ct2
      in
      match CCList.assoc_opt ~eq other_type substitutions with
      | Some replacement -> replacement
      | None -> Ast_builder.Default.evar ~loc (to_encoder_name lid))
  | _ ->
      Location.raise_errorf ~loc "Cannot construct decoder for %s"
        (string_of_core_type typ)

let implementation_generator ~(loc : location) ~rec_flag ~substitutions
    type_decl : expression =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  let _name = to_encoder_name type_decl.ptype_name.txt in
  let imple_expr =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest -> expr_of_typ ~substitutions manifest
    | Ptype_variant _cstrs, None ->
        failwith "NYI" (* expr_of_variant ~loc ~substitutions cstrs *)
    | Ptype_record _label_decs, _ ->
        failwith "NYI" (* expr_of_record ~substitutions ~loc label_decs *)
    | Ptype_open, _ -> Location.raise_errorf ~loc "Unhandled open"
    | _ -> Location.raise_errorf ~loc "Unhandled mystery"
  in
  match rec_flag with Nonrecursive -> imple_expr | Recursive -> failwith "NYI"
(* wrap_as_aux ~loc ~name ~expr:imple_expr *)

let single_type_decoder_gen ~(loc : location) ~rec_flag type_decl :
    structure_item list =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  let name = to_encoder_name type_decl.ptype_name.txt in
  let substitutions =
    match rec_flag with
    | Nonrecursive -> []
    | Recursive ->
        [
          ( core_type_of_type_declaration type_decl,
            Ast_builder.Default.evar ~loc (name ^ "_aux") );
        ]
  in
  let imple =
    implementation_generator ~loc ~rec_flag ~substitutions type_decl
  in
  let name = to_encoder_name type_decl.ptype_name.txt in
  [%str let [%p Ast_builder.Default.pvar ~loc name] = [%e imple]]

let str_gens ~(loc : location) ~(path : label)
    ((rec_flag : rec_flag), type_decls) : structure_item list =
  let _path = path in
  match (really_recursive rec_flag type_decls, type_decls) with
  | Nonrecursive, _ ->
      List.(flatten (map (single_type_decoder_gen ~loc ~rec_flag) type_decls))
  | Recursive, [ type_decl ] ->
      Utils.wrap_27 @@ single_type_decoder_gen ~loc ~rec_flag type_decl
  | Recursive, _type_decls -> failwith "NYI"
(* Utils.wrap_27 *)
(* @@ mutual_rec_fun_gen ~substitutions:[] ~loc type_decls *)
(* @ fix_mutual_rec_funs ~loc type_decls *)
