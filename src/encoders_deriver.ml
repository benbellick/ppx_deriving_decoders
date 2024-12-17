open Ppxlib

let to_encoder_name i = i ^ "_encoder"

let rec flatten_longident ~loc = function
  | Lident txt -> txt
  | Ldot (longident, txt) -> flatten_longident longident ~loc ^ "." ^ txt
  | Lapply (fst, snd) ->
      Location.raise_errorf ~loc "Cannot handle functors:%s (%s)"
        (flatten_longident ~loc fst)
        (flatten_longident ~loc snd)

let longident_to_encoder_name ~loc =
  CCFun.(to_encoder_name % flatten_longident ~loc)

let name_to_encoder_name (i : string loc) = to_encoder_name i.txt

let rec expr_of_typ (typ : core_type) : expression =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  match typ with
  | [%type: unit] | [%type: unit] -> Ast_builder.Default.evar ~loc "E.null"
  | [%type: int] -> Ast_builder.Default.evar ~loc "E.int"
  | [%type: int32] | [%type: Int32.t] ->
      let int_enc = Ast_builder.Default.evar ~loc "E.int" in
      [%expr fun i -> i |> Int32.to_int |> [%e int_enc]]
  | [%type: int64] | [%type: Int64.t] ->
      let int_enc = Ast_builder.Default.evar ~loc "E.int" in
      [%expr fun i -> i |> Int64.to_int |> [%e int_enc]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      let int_enc = Ast_builder.Default.evar ~loc "E.int" in
      [%expr fun i -> i |> Nativeint.to_int |> [%e int_enc]]
  | [%type: float] -> Ast_builder.Default.evar ~loc "E.float"
  | [%type: bool] -> Ast_builder.Default.evar ~loc "E.bool"
  | [%type: char] -> [%expr fun c -> E.string (String.make 1 c)]
  | [%type: string] | [%type: String.t] ->
      Ast_builder.Default.evar ~loc "E.string"
  | [%type: bytes] | [%type: Bytes.t] ->
      Location.raise_errorf ~loc "Cannot construct an encoder for bytes"
  | [%type: [%t? inner_typ] list] ->
      let list_encoder = Ast_builder.Default.evar ~loc "E.list" in
      let sub_expr = expr_of_typ inner_typ in
      Ast_helper.Exp.apply ~loc list_encoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] array] ->
      let array_encoder = Ast_builder.Default.evar ~loc "E.array" in
      let sub_expr = expr_of_typ inner_typ in
      Ast_helper.Exp.apply ~loc array_encoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] option] ->
      let opt_encoder = Ast_builder.Default.evar ~loc "E.nullable" in
      let sub_expr = expr_of_typ (* ~substitutions *) inner_typ in
      Ast_helper.Exp.apply ~loc opt_encoder [ (Nolabel, sub_expr) ]
  | { ptyp_desc = Ptyp_tuple typs; _ } -> expr_of_tuple ~loc typs
  | { ptyp_desc = Ptyp_var var; _ } ->
      Ast_builder.Default.evar ~loc @@ to_encoder_name var
  | { ptyp_desc = Ptyp_constr ({ txt = Lident lid; _ }, []); _ } ->
      (* The assumption here is that if we get to this point, this type is recursive, and
         we just assume that we already have an encoder available.
         TODO: Is this really the case?
      *)
      Ast_builder.Default.evar ~loc (to_encoder_name lid)
  | { ptyp_desc = Ptyp_constr ({ txt = longident; loc }, args); _ } ->
      let cstr_dec =
        Ast_builder.Default.evar ~loc
        @@ longident_to_encoder_name ~loc longident
      in

      let arg_decs = CCList.map expr_of_typ args in
      Ast_builder.Default.eapply ~loc cstr_dec arg_decs
  | { ptyp_desc = Ptyp_arrow _; _ } ->
      Location.raise_errorf ~loc
        "Cannot construct encoder for %s: cannot encode functions"
        (string_of_core_type typ)
  | { ptyp_desc = Ptyp_object _; _ } ->
      Location.raise_errorf ~loc
        "Cannot construct encoder for %s: cannot (yet) encode objects"
        (string_of_core_type typ)
  | _ ->
      Location.raise_errorf ~loc "Cannot construct encoder for %s"
        (string_of_core_type typ)

and expr_of_tuple ~loc (* ~substitutions ?lift *) typs =
  (* Want to take type a * b * c  and produce
     fun (arg1,arg2,arg3) -> E.list E.value [E.a arg1; E.b arg2; E.c arg3]
  *)
  let typ_encoders_exprs = List.map expr_of_typ (* ~substitutions *) typs in
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

  let encoder_result = [%expr E.list E.value [%e encoded_args]] in
  [%expr [%e encoder_result]]

and expr_of_record ~loc (* ~substitutions ?lift *) label_decls =
  (* To help understand what this function is doing, imagine we had
     a type [type t = {i : int; s : string}]. Then this will render the encoder:
     let t_encoder : t E.encoder =
     fun {i; s} -> E.obj [("i", int i); ("s", string s)]
  *)
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
  encode_all

and expr_of_constr_arg ~loc (arg : constructor_arguments) =
  match arg with
  | Pcstr_tuple tups -> expr_of_tuple ~loc tups
  | Pcstr_record labl_decls -> expr_of_record ~loc labl_decls

and expr_of_constr_decl
    ({ pcd_args; pcd_loc = loc; _ } as cstr_decl : constructor_declaration) =
  (* We assume at this point that the decomposition into indiviaul fields is handled by caller *)
  let cstr_name = Ast_builder.Default.estring ~loc cstr_decl.pcd_name.txt in
  let encoded_args =
    match pcd_args with
    | Pcstr_tuple [] -> [%expr E.null]
    | Pcstr_tuple [ single ] ->
        let enc = expr_of_typ single in
        let on = Ast_builder.Default.evar ~loc (Utils.argn 0) in
        [%expr [%e enc] [%e on]]
    | _ -> expr_of_constr_arg ~loc pcd_args
  in

  [%expr E.obj [ ([%e cstr_name], [%e encoded_args]) ]]

and expr_of_variant ~loc cstrs =
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
      | Pcstr_tuple [ _tuple ] -> Some (pvar ~loc (Utils.argn 0))
      | Pcstr_tuple tuples ->
          Some
            (ppat_tuple ~loc
            @@ CCList.mapi (fun i _tup -> pvar ~loc (Utils.argn i)) tuples)
      | Pcstr_record lbl_decls ->
          let arg_fields =
            CCList.map
              (fun { pld_name; _ } ->
                ( { txt = Lident pld_name.txt; loc },
                  Ast_builder.Default.pvar ~loc
                    (*TODO: is this right loc*) pld_name.txt ))
              lbl_decls
          in
          Some (Ast_builder.Default.ppat_record ~loc arg_fields Closed)
    in

    let vpat =
      ppat_construct ~loc (Utils.lident_of_constructor_decl cstr) inner_pattern
    in
    let enc_expression = expr_of_constr_decl cstr in
    case ~lhs:vpat ~guard:None ~rhs:enc_expression
  in
  let cases = List.map to_case cstrs in
  pexp_function ~loc cases

let implementation_generator ~(loc : location) type_decl : expression =
  let _name = to_encoder_name type_decl.ptype_name.txt in
  let imple_expr =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest -> (
        let expr = expr_of_typ manifest in
        match manifest with
        | { ptyp_desc = Ptyp_tuple typs; _ } ->
            (* In the case of a top level tuple, we need to explicitly wrap in a lambda with
               the arguments
            *)
            let args =
              Ast_builder.Default.ppat_tuple ~loc
              @@ CCList.mapi
                   (fun i _typ -> Ast_builder.Default.pvar ~loc (Utils.argn i))
                   typs
            in
            [%expr fun [%p args] -> [%e expr]]
        | _ -> expr)
    | Ptype_variant cstrs, None -> expr_of_variant ~loc cstrs
    | Ptype_record label_decs, _ ->
        (* And in the case of a top-level record, we also need to explicitly wrap in a lambda with args *)
        let arg_fields =
          CCList.map
            (fun { pld_name; _ } ->
              ( { txt = Lident pld_name.txt; loc },
                Ast_builder.Default.pvar ~loc
                  (*TODO: is this right loc*) pld_name.txt ))
            label_decs
        in
        let args = Ast_builder.Default.ppat_record ~loc arg_fields Closed in
        let expr = expr_of_record ~loc label_decs in
        [%expr fun [%p args] -> [%e expr]]
    | Ptype_open, _ -> Location.raise_errorf ~loc "Unhandled open"
    | _ -> Location.raise_errorf ~loc "Unhandled mystery"
  in
  imple_expr

let single_type_encoder_gen ~(loc : location) type_decl =
  let imple = implementation_generator ~loc type_decl in
  let name = to_encoder_name type_decl.ptype_name.txt in
  let pat = Ast_builder.Default.pvar ~loc name in
  let params =
    (* TODO: can we drop the non type vars? What are these? *)
    CCList.filter_map
      (fun (param, _) ->
        match param.ptyp_desc with Ptyp_var var -> Some var | _ -> None)
      type_decl.ptype_params
  in
  let args =
    CCList.rev
    @@ CCList.map
         (fun param -> Ast_builder.Default.pvar ~loc (to_encoder_name param))
         params
  in
  let imple =
    (* We need the type variables to become arguments *)
    CCList.fold_left
      (fun impl arg -> [%expr fun [%p arg] -> [%e impl]])
      imple args
  in
  Ast_builder.Default.value_binding ~loc ~pat ~expr:imple
(* [%str let [%p Ast_builder.Default.pvar ~loc name] = [%e imple]] *)

let str_gens ~(loc : location) ~(path : label)
    ((rec_flag : rec_flag), type_decls) : structure_item list =
  let _path = path in
  let rec_flag = really_recursive rec_flag type_decls in

  (* CCList.flat_map (single_type_encoder_gen ~loc ~rec_flag) type_decls *)
  match (really_recursive rec_flag type_decls, type_decls) with
  | Nonrecursive, _ ->
      [
        (Ast_builder.Default.pstr_value ~loc Nonrecursive
        @@ List.(map (single_type_encoder_gen ~loc) type_decls));
      ]
  | Recursive, type_decls ->
      [
        (Ast_builder.Default.pstr_value ~loc Recursive
        @@ List.(map (single_type_encoder_gen ~loc) type_decls));
      ]
