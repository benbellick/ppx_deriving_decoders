open Ppxlib

let to_encoder_name i = i ^ "_encoder"

let rec expr_of_typ (typ : core_type) : expression =
  (* ~(substitutions : (core_type * expression) list) *)
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
      let sub_expr = expr_of_typ (* ~substitutions *) inner_typ in
      Ast_helper.Exp.apply ~loc list_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] array] ->
      let array_decoder = Ast_builder.Default.evar ~loc "E.array" in
      let sub_expr = expr_of_typ (* ~substitutions *) inner_typ in
      Ast_helper.Exp.apply ~loc array_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] option] ->
      let opt_decoder = Ast_builder.Default.evar ~loc "E.nullable" in
      let sub_expr = expr_of_typ (* ~substitutions *) inner_typ in
      Ast_helper.Exp.apply ~loc opt_decoder [ (Nolabel, sub_expr) ]
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
      expr_of_tuple (* ~substitutions *) ~loc typs
  (* | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } -> _ *)
  (* | { ptyp_desc = Ptyp_alias _; _ } -> *)
  (*     failwith *)
  (*       (Format.sprintf "This alias was a failure...: %s\n" *)
  (*          (string_of_core_type typ)) *)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident _lid; _ }, []); _ } as _other_type
    ->
      failwith "NYI"
      (* ( *)
      (* (\* In the case where our type is truly recursive, we need to instead do `type_aux ()` *\) *)
      (* let eq (ct1 : core_type) (ct2 : core_type) = *)
      (*   (\* TODO: This is a terrible way to compare the types... *\) *)
      (*   string_of_core_type ct1 = string_of_core_type ct2 *)
      (* in *)
      (* match CCList.assoc_opt ~eq other_type substitutions with *)
      (* | Some replacement -> replacement *)
      (* | None -> Ast_builder.Default.evar ~loc (to_encoder_name lid)) *)
  | _ ->
      Location.raise_errorf ~loc "Cannot construct decoder for %s"
        (string_of_core_type typ)

and expr_of_tuple ~loc (* ~substitutions ?lift *) typs =
  (* Want to take type a * b * c  and produce
     fun (arg1,arg2,arg3) -> E.list E.value [E.a arg1; E.b arg2; E.c arg3]
  *)
  let typ_encoders_exprs = List.map expr_of_typ (* ~substitutions *) typs in
  let pargs =
    CCList.mapi
      (fun idx _typ -> Ast_builder.Default.pvar ~loc @@ Utils.argn idx)
      typs
  in
  let eargs =
    CCList.mapi
      (fun idx _typ -> Ast_builder.Default.evar ~loc @@ Utils.argn idx)
      typs
  in
  let encoded_args =
    Ast_builder.Default.elist ~loc
    @@ CCList.map2
         (fun encoder arg -> [%expr [%e encoder] [%e arg]])
         typ_encoders_exprs eargs
  in
  let encoder_arg = Ast_builder.Default.ppat_tuple ~loc pargs in
  let encoder_result = [%expr E.list E.value [%e encoded_args]] in
  [%expr fun [%p encoder_arg] -> [%e encoder_result]]

and expr_of_record ~loc (* ~substitutions ?lift *) label_decls =
  (* To help understand what this function is doing, imagine we had
     a type [type t = {i : int; s : string}]. Then this will render the encoder:
     let t_encoder : t E.encoder =
     fun {i; s} -> E.obj [("i", int i); ("s", string s)]
  *)
  let arg_fields =
    CCList.map
      (fun { pld_name; _ } ->
        ( { txt = Lident pld_name.txt; loc },
          Ast_builder.Default.pvar ~loc (*TODO: is this right loc*) pld_name.txt
        ))
      label_decls
  in
  let parg = Ast_builder.Default.ppat_record ~loc arg_fields Closed in
  let encode_field { pld_name; pld_type; _ } =
    Ast_builder.Default.(
      pexp_tuple ~loc
        [
          estring ~loc pld_name.txt;
          eapply ~loc (expr_of_typ pld_type) [ evar ~loc pld_name.txt ];
        ])
  in
  let encode_all =
    let open Ast_builder.Default in
    eapply ~loc (evar ~loc "E.obj")
    @@ [ elist ~loc (CCList.map encode_field label_decls) ]
  in
  Ast_builder.Default.pexp_fun ~loc Nolabel None parg encode_all

and expr_of_constr_arg ~loc (* ~cstr *)
    (* ~substitutions *) (arg : constructor_arguments) =
  match arg with
  | Pcstr_tuple tups -> expr_of_tuple (* ~substitutions ~lift:cstr *) ~loc tups
  | Pcstr_record labl_decls ->
      expr_of_record ~loc (* ~substitutions ~lift:cstr *) labl_decls

and expr_of_constr_decl (* ~substitutions *)
    ({ pcd_args; pcd_loc = loc; _ } as cstr_decl : constructor_declaration) =
  (* We assume at this point that the decomposition into indiviaul fields is handled by caller *)
  if pcd_args = Pcstr_tuple [] then
    let cstr = Utils.lident_of_constructor_decl cstr_decl in
    let cstr = Ast_builder.Default.pexp_construct ~loc cstr None in
    [%expr succeed [%e cstr]]
  else
    (* let cstr = Utils.lident_of_constructor_decl cstr_decl in *)
    let sub_expr =
      expr_of_constr_arg (* ~substitutions *) ~loc (* ~cstr *) pcd_args
    in
    sub_expr

and expr_of_variant ~loc (* ~substitutions *) cstrs =
  (* Producing from type `A | B of b | C of c`
     to
     function
     | A -> {"A":null}
     | B b -> {"B": b_encoder b}
     | C c - {"C": c_encoder c}
  *)
  let open Ast_builder.Default in
  let to_case (cstr : constructor_declaration) =
    let inner_pattern =
      match cstr.pcd_args with
      | Pcstr_tuple [] -> None
      | Pcstr_tuple tuples ->
          Some
            (plist ~loc
            @@ CCList.mapi (fun i _tup -> pvar ~loc (Utils.argn i)) tuples)
      | Pcstr_record lbl_decls ->
          Some
            (plist ~loc
            @@ CCList.mapi (fun i _decl -> pvar ~loc (Utils.argn i)) lbl_decls)
    in
    let vpat =
      ppat_construct ~loc (Utils.lident_of_constructor_decl cstr) inner_pattern
    in
    let enc_expression = expr_of_constr_decl (* ~substitutions *) cstr in
    case ~lhs:vpat ~guard:None ~rhs:enc_expression
  in
  let cases = List.map to_case cstrs in
  let encode_by_field = pexp_function ~loc cases in
  [%expr
    let open E in
    [%e encode_by_field]]

let implementation_generator ~(loc : location) ~rec_flag (* ~substitutions *)
    type_decl : expression =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  let _name = to_encoder_name type_decl.ptype_name.txt in
  let imple_expr =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest -> expr_of_typ (* ~substitutions *) manifest
    | Ptype_variant cstrs, None ->
        expr_of_variant ~loc (* ~substitutions *) cstrs
    | Ptype_record label_decs, _ ->
        expr_of_record (* ~substitutions *) ~loc label_decs
    | Ptype_open, _ -> Location.raise_errorf ~loc "Unhandled open"
    | _ -> Location.raise_errorf ~loc "Unhandled mystery"
  in
  match rec_flag with Nonrecursive -> imple_expr | Recursive -> failwith "NYI"
(* wrap_as_aux ~loc ~name ~expr:imple_expr *)

let single_type_decoder_gen ~(loc : location) ~rec_flag type_decl :
    structure_item list =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  (* let name = to_encoder_name type_decl.ptype_name.txt in *)
  (* let substitutions = *)
  (*   match rec_flag with *)
  (*   | Nonrecursive -> [] *)
  (*   | Recursive -> *)
  (*       [ *)
  (*         ( core_type_of_type_declaration type_decl, *)
  (*           Ast_builder.Default.evar ~loc (name ^ "_aux") ); *)
  (*       ] *)
  (* in *)
  let imple =
    implementation_generator ~loc ~rec_flag (* ~substitutions *) type_decl
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
