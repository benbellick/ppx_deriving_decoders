open Ppxlib

let apply_substitution ~orig ~substi =
  let mapper =
    object
      inherit Ast_traverse.map as super

      method! expression expr =
        let eq e1 e2 =
          (* TODO: terrible way to compare expressions *)
          Pprintast.string_of_expression e1 = Pprintast.string_of_expression e2
        in
        if eq expr orig then substi else super#expression expr
    end
  in
  mapper#expression

let to_decoder_name i = i ^ "_decoder"

let rec flatten_longident ~loc = function
  | Lident txt -> txt
  | Ldot (longident, txt) -> flatten_longident ~loc longident ^ "." ^ txt
  | Lapply (fst, snd) ->
      Location.raise_errorf ~loc "Cannot handle functors:%s (%s)"
        (flatten_longident ~loc fst)
        (flatten_longident ~loc snd)

let longident_to_decoder_name ~loc =
  CCFun.(to_decoder_name % flatten_longident ~loc)

let name_to_decoder_name (i : string loc) = to_decoder_name i.txt

let decoder_pvar_of_type_decl type_decl =
  Ast_builder.Default.pvar ~loc:type_decl.ptype_name.loc
    (name_to_decoder_name type_decl.ptype_name)

let decoder_evar_of_type_decl type_decl =
  Ast_builder.Default.evar ~loc:type_decl.ptype_name.loc
    (name_to_decoder_name type_decl.ptype_name)

(** We take an expr implementation with name NAME and turn it into:
    let rec NAME_AUX = fun () -> expr in NAME_AUX ().

    This is so that we can later do:


    let NAME =
      let rec NAME_AUX () = expr in NAME_AUX ()

    Thus fixing the recursive type issue. 
 *)
let wrap_as_aux ~loc ~name ~expr =
  let open Ast_builder.Default in
  let aux_fn_p = pvar ~loc (name ^ "_aux") in
  [%expr D.fix (fun [%p aux_fn_p] -> [%e expr])]

let pexp_fun_multiarg ~loc fun_imple (args : pattern list) =
  (* Making something like fun arg1 arg2 ... -> fun_imple *)
  let folder f arg = Ast_builder.Default.pexp_fun ~loc Nolabel None arg f in
  (* TODO: remove inefficient list reversal  *)
  let args_rev = List.rev args in
  CCList.fold_left folder fun_imple args_rev

let rec expr_of_typ (typ : core_type)
    ~(substitutions : (core_type * expression) list) : expression =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  match typ with
  | [%type: unit] | [%type: unit] -> Ast_builder.Default.evar ~loc "D.null"
  | [%type: int] -> Ast_builder.Default.evar ~loc "D.int"
  | [%type: int32] | [%type: Int32.t] ->
      let int_dec = Ast_builder.Default.evar ~loc "D.int" in
      [%expr
        let open D.Infix in
        [%e int_dec] >|= Int32.of_int]
  | [%type: int64] | [%type: Int64.t] ->
      let int_dec = Ast_builder.Default.evar ~loc "D.int" in
      [%expr
        let open D.Infix in
        [%e int_dec] >|= Int64.of_int]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      let int_dec = Ast_builder.Default.evar ~loc "D.int" in
      [%expr
        let open D.Infix in
        [%e int_dec] >|= Nativeint.of_int]
  | [%type: float] -> Ast_builder.Default.evar ~loc "D.float"
  | [%type: bool] -> Ast_builder.Default.evar ~loc "D.bool"
  | [%type: char] ->
      [%expr
        let open D.Infix in
        D.string >>= fun s ->
        if String.length s = 1 then D.succeed @@ String.get s 0
        else D.fail "Expected a string of length 1"]
  | [%type: string] | [%type: String.t] ->
      Ast_builder.Default.evar ~loc "D.string"
  | [%type: bytes] | [%type: Bytes.t] ->
      failwith "Cannot handle Bytes" (* TODO: figure out strategy *)
  | [%type: [%t? inner_typ] list] ->
      let list_decoder = Ast_builder.Default.evar ~loc "D.list" in
      let sub_expr = expr_of_typ ~substitutions inner_typ in
      Ast_helper.Exp.apply ~loc list_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] array] ->
      let array_decoder = Ast_builder.Default.evar ~loc "D.array" in
      let sub_expr = expr_of_typ ~substitutions inner_typ in
      Ast_helper.Exp.apply ~loc array_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] option] ->
      let opt_decoder = Ast_builder.Default.evar ~loc "D.nullable" in
      let sub_expr = expr_of_typ ~substitutions inner_typ in
      Ast_helper.Exp.apply ~loc opt_decoder [ (Nolabel, sub_expr) ]
  | { ptyp_desc = Ptyp_tuple typs; _ } -> expr_of_tuple ~substitutions ~loc typs
  (* | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } -> _ *)
  (* | { ptyp_desc = Ptyp_alias _; _ } -> *)
  (*     failwith *)
  (*       (Format.sprintf "This alias was a failure...: %s\n" *)
  (*          (string_of_core_type typ)) *)
  | { ptyp_desc = Ptyp_constr ({ txt = longident; loc = typ_loc }, []); _ } as
    other_type -> (
      (* In the case where our type is truly recursive, we need to instead do `type_aux ()` *)
      let eq (ct1 : core_type) (ct2 : core_type) =
        (* TODO: This is a terrible way to compare the types... *)
        string_of_core_type ct1 = string_of_core_type ct2
      in
      match CCList.assoc_opt ~eq other_type substitutions with
      | Some replacement -> replacement
      | None ->
          Ast_builder.Default.evar ~loc
            (longident_to_decoder_name ~loc:typ_loc longident))
  | { ptyp_desc = Ptyp_var var; _ } ->
      Ast_builder.Default.evar ~loc @@ to_decoder_name var
  | { ptyp_desc = Ptyp_constr ({ txt = longident; loc }, args); _ } ->
      let cstr_dec =
        Ast_builder.Default.evar ~loc
        @@ longident_to_decoder_name ~loc longident
      in

      let arg_decs = CCList.map (expr_of_typ ~substitutions) args in
      Ast_builder.Default.eapply ~loc cstr_dec arg_decs
      (* Location.raise_errorf ~loc "Cannot constructor decoder for %s" *)
      (*   (string_of_core_type typ) *)
  | _ ->
      Location.raise_errorf ~loc "Cannot construct decoder for %s"
        (string_of_core_type typ)

and expr_of_tuple ~loc ~substitutions ?lift typs =
  (* To help understand what this function is doing, imagine we had
     a type [type t = int * string * bool]. Then this will render the decoder:
     let t_decoder : t D.decoder =
     let open D in
     let ( >>=:: ) fst rest = uncons rest fst in
     int >>=:: fun arg1 ->
     string >>=:: fun arg2 ->
     bool >>=:: fun arg3 -> succeed (arg1, arg2, arg3)

     Though if lift is present (lift is a type constructor), we will instead get:
     let t_decoder : t D.decoder =
     let open D in
     let ( >>=:: ) fst rest = uncons rest fst in
     int >>=:: fun arg1 ->
     string >>=:: fun arg2 ->
     bool >>=:: fun arg3 -> succeed (lift (arg1, arg2, arg3))
  *)
  let typ_decoder_exprs = List.map (expr_of_typ ~substitutions) typs in
  let base =
    (* Consists of the initial setup partial function def, which is the inport and local definition,
       as well as a running count of how many arguments there are *)
    ( (fun body ->
        [%expr
          let open D in
          let ( >>=:: ) fst rest = uncons rest fst in
          [%e body]]),
      0 )
  in
  let fn_builder (partial_expr, i) next_decoder =
    let var = Utils.argn i in
    let var_pat = Ast_builder.Default.pvar ~loc var in
    ( (fun body ->
        partial_expr
          [%expr [%e next_decoder] >>=:: fun [%p var_pat] -> [%e body]]),
      i + 1 )
  in
  let complete_partial_expr, var_count =
    List.fold_left fn_builder base typ_decoder_exprs
  in
  let var_names = CCList.init var_count Utils.argn in
  let var_tuple =
    let expr_list =
      List.map (fun s -> [%expr [%e Ast_builder.Default.evar ~loc s]]) var_names
    in
    Ast_builder.Default.pexp_tuple ~loc expr_list
  in
  match lift with
  | Some lift ->
      let var_tuple_lift =
        Ast_builder.Default.pexp_construct ~loc lift
          (Some [%expr [%e var_tuple]])
      in
      complete_partial_expr [%expr succeed [%e var_tuple_lift]]
  | None -> complete_partial_expr [%expr succeed [%e var_tuple]]

and expr_of_constr_decl ~substitutions
    ({ pcd_args; pcd_loc = loc; _ } as cstr_decl : constructor_declaration) =
  (* We assume at this point that the decomposition into indiviaul fields is handled by caller *)
  match pcd_args with
  | Pcstr_tuple [] ->
      let cstr = Utils.lident_of_constructor_decl cstr_decl in
      let cstr = Ast_builder.Default.pexp_construct ~loc cstr None in
      [%expr succeed [%e cstr]]
  | Pcstr_tuple [ single ] ->
      let cstr = Utils.lident_of_constructor_decl cstr_decl in
      let arg_e = Ast_builder.Default.evar ~loc "arg" in
      let arg_p = Ast_builder.Default.pvar ~loc "arg" in
      let cstr = Ast_builder.Default.pexp_construct ~loc cstr (Some arg_e) in
      let single_dec = expr_of_typ single ~substitutions in
      [%expr [%e single_dec] >|= fun [%p arg_p] -> [%e cstr]]
  | _ ->
      let cstr = Utils.lident_of_constructor_decl cstr_decl in
      let sub_expr = expr_of_constr_arg ~substitutions ~loc ~cstr pcd_args in
      sub_expr

and expr_of_constr_arg ~loc ~cstr ~substitutions (arg : constructor_arguments) =
  match arg with
  | Pcstr_tuple tups -> expr_of_tuple ~substitutions ~lift:cstr ~loc tups
  | Pcstr_record labl_decls ->
      expr_of_record ~loc ~substitutions ~lift:cstr labl_decls

and expr_of_record ~loc ~substitutions ?lift label_decls =
  (* To help understand what this function is doing, imagine we had
     a type [type t = {i : int; s : string}]. Then this will render the decoder:
     let t_decoder : t D.decoder =
     let open D in
     let open D.Infix in
     let* i = field "i" int in
     let* s = field "s" string in
     succeed {i; s}

     or optionally:
     let t_decoder : t D.decoder =
     let open D in
     let open D.Infix in
     let* i = field "i" int in
     let* s = field "s" string in
     succeed (lift {i; s})
  *)
  let base
      (* Consists of the initial setup partial function def, which is the inport and local definition,
         as well as a running count of how many arguments there are *)
        body =
    [%expr
      let open D in
      let open D.Infix in
      [%e body]]
  in

  let fn_builder partial_expr ({ pld_name; pld_type; _ } : label_declaration) =
    let subexpr = expr_of_typ ~substitutions pld_type in
    let var_pat = Ast_builder.Default.pvar ~loc:pld_name.loc pld_name.txt in
    (* TODO correct loc?  *)
    let str = Ast_builder.Default.estring ~loc:pld_name.loc pld_name.txt in
    fun body ->
      partial_expr
        [%expr
          let* [%p var_pat] = field [%e str] [%e subexpr] in
          [%e body]]
  in

  let complete_partial_expr = List.fold_left fn_builder base label_decls in
  let var_names =
    CCList.map
      (fun (label_decl : label_declaration) ->
        Ast_builder.Default.
          ( { txt = Longident.Lident label_decl.pld_name.txt; loc },
            evar ~loc label_decl.pld_name.txt ))
      label_decls
  in
  let record = Ast_builder.Default.pexp_record ~loc var_names None in
  match lift with
  | None -> complete_partial_expr [%expr succeed [%e record]]
  | Some lift ->
      let record_lift =
        Ast_builder.Default.pexp_construct ~loc lift (Some [%expr [%e record]])
      in
      complete_partial_expr [%expr succeed [%e record_lift]]

let expr_of_variant ~loc ~substitutions cstrs =
  let open Ast_builder.Default in
  let match_all_case =
    let match_all_pvar = pvar ~loc "any" in
    let match_all_evar = evar ~loc "any" in
    case ~lhs:match_all_pvar ~guard:None
      ~rhs:
        [%expr
          D.fail @@ Printf.sprintf "Unrecognized field: %s" [%e match_all_evar]]
  in
  let to_case (cstr : constructor_declaration) =
    let name_pattern = pstring ~loc cstr.pcd_name.txt in
    let dec_expression = expr_of_constr_decl ~substitutions cstr in
    case ~lhs:name_pattern ~guard:None ~rhs:dec_expression
  in

  let cases = List.map to_case cstrs in
  let cases = List.append cases [ match_all_case ] in
  let decode_by_field = pexp_function ~loc cases in
  [%expr
    let open D in
    single_field [%e decode_by_field]]

let implementation_generator ~(loc : location) ~rec_flag ~substitutions
    type_decl : expression =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  let name = name_to_decoder_name type_decl.ptype_name in
  let imple_expr =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest -> expr_of_typ ~substitutions manifest
    | Ptype_variant cstrs, None -> expr_of_variant ~loc ~substitutions cstrs
    | Ptype_record label_decs, _ ->
        expr_of_record ~substitutions ~loc label_decs
    | Ptype_open, _ -> Location.raise_errorf ~loc "Unhandled open"
    | _ -> Location.raise_errorf ~loc "Unhandled mystery"
  in
  match rec_flag with
  | Nonrecursive -> imple_expr
  | Recursive -> wrap_as_aux ~loc ~name ~expr:imple_expr

let single_type_decoder_gen ~(loc : location) ~rec_flag type_decl :
    structure_item list =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  let name = name_to_decoder_name type_decl.ptype_name in

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
  let name = name_to_decoder_name type_decl.ptype_name in
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
         (fun param -> Ast_builder.Default.pvar ~loc (to_decoder_name param))
         params
  in
  let imple =
    (* We need the type variables to become arguments *)
    CCList.fold_left
      (fun impl arg -> [%expr fun [%p arg] -> [%e impl]])
      imple args
  in
  [%str let [%p Ast_builder.Default.pvar ~loc name] = [%e imple]]

let rec mutual_rec_fun_gen ~loc
    ~substitutions
     (* These only generate the decoder in terms of one another, prior to utilizing `fix` *)
    (type_decls : type_declaration list) =
  let open Ast_builder.Default in
  match type_decls with
  | type_decl :: rest ->
      let var =
        pvar ~loc:type_decl.ptype_name.loc
          (name_to_decoder_name type_decl.ptype_name)
      in
      let substitutions =
        match really_recursive Recursive [ type_decl ] with
        | Recursive ->
            let name = name_to_decoder_name type_decl.ptype_name in
            let substi = Ast_builder.Default.evar ~loc (name ^ "_aux") in
            let new_substitution =
              (core_type_of_type_declaration type_decl, substi)
            in
            (* TODO this should be bundled into a module *)
            let updated_orig_substitutions =
              let open CCList.Infix in
              let+ typ, expr = substitutions in
              let orig = decoder_evar_of_type_decl type_decl in
              (typ, apply_substitution ~orig ~substi expr)
            in
            new_substitution :: updated_orig_substitutions
        | Nonrecursive -> substitutions
      in
      let imple =
        implementation_generator ~loc ~rec_flag:Recursive ~substitutions
          type_decl
      in
      let args =
        (* If we are on the last decoder, it needs to take itself in as a param *)
        if rest = [] then [ var ]
        else
          List.map
            (fun type_decl ->
              let name = name_to_decoder_name type_decl.ptype_name in
              pvar ~loc:type_decl.ptype_name.loc name)
            rest
      in
      let imple_as_lambda = pexp_fun_multiarg ~loc imple args in
      let dec = [%stri let [%p var] = [%e imple_as_lambda]] in
      let substi =
        pexp_apply ~loc
          (evar ~loc (name_to_decoder_name type_decl.ptype_name))
          (List.map
             (fun decl ->
               (Nolabel, evar ~loc (name_to_decoder_name decl.ptype_name)))
             rest)
      in
      let new_substitution =
        (core_type_of_type_declaration type_decl, substi)
      in
      (* TODO this should be bundled into a module *)
      let updated_orig_substitutions =
        let open CCList.Infix in
        let+ typ, expr = substitutions in
        let orig = decoder_evar_of_type_decl type_decl in
        (typ, apply_substitution ~orig ~substi expr)
      in

      let substitutions = new_substitution :: updated_orig_substitutions in
      dec :: mutual_rec_fun_gen ~loc ~substitutions rest
  | [] -> []

let rec fix_mutual_rec_funs ~loc type_decls =
  let open Ast_builder.Default in
  match type_decls with
  | [] -> []
  | [ type_decl ] ->
      let var_p = decoder_pvar_of_type_decl type_decl in
      let var_e = decoder_evar_of_type_decl type_decl in
      [ [%stri let [%p var_p] = D.fix [%e var_e]] ]
  | type_decl :: rest ->
      let var_p = decoder_pvar_of_type_decl type_decl in
      let var_e = decoder_evar_of_type_decl type_decl in
      let args =
        List.map (fun decl -> (Nolabel, decoder_evar_of_type_decl decl)) rest
      in
      let appli = pexp_apply ~loc var_e args in
      let dec = [%stri let [%p var_p] = [%e appli]] in
      (* TODO: inefficient list append *)
      fix_mutual_rec_funs ~loc rest @ [ dec ]

let str_gens ~(loc : location) ~(path : label)
    ((rec_flag : rec_flag), type_decls) : structure_item list =
  let _path = path in
  match (really_recursive rec_flag type_decls, type_decls) with
  | Nonrecursive, _ ->
      CCList.flat_map (single_type_decoder_gen ~loc ~rec_flag) type_decls
  | Recursive, [ type_decl ] ->
      Utils.wrap_27 @@ single_type_decoder_gen ~loc ~rec_flag type_decl
  | Recursive, _type_decls ->
      Utils.wrap_27
      @@ mutual_rec_fun_gen ~substitutions:[] ~loc type_decls
      @ fix_mutual_rec_funs ~loc type_decls
