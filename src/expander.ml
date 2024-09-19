open Ppxlib
module D = Decoders_yojson.Safe.Decode

let to_decoder_name i = i ^ "_decoder"

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

let lident_of_constructor_decl (cd : constructor_declaration) =
  let loc = cd.pcd_name.loc in
  let name = cd.pcd_name.txt in
  Ast_builder.Default.Located.lident ~loc name (* Convert to lident *)

let rec expr_of_typ (typ : core_type)
    ~(substitutions : (core_type * expression) list) : expression =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  match typ with
  | [%type: unit] | [%type: unit] -> Ast_builder.Default.evar ~loc "D.null"
  | [%type: int] -> Ast_builder.Default.evar ~loc "D.int"
  | [%type: int32]
  | [%type: Int32.t]
  | [%type: int64]
  | [%type: Int64.t]
  | [%type: nativeint]
  | [%type: Nativeint.t] ->
      failwith "Cannot yet handle any int-like but int"
  | [%type: float] -> Ast_builder.Default.evar ~loc "D.float"
  | [%type: bool] -> Ast_builder.Default.evar ~loc "D.bool"
  | [%type: char] ->
      failwith "Cannot directly handle character; please cast to string first"
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
  | { ptyp_desc = Ptyp_constr ({ txt = Lident lid; _ }, []); _ } as other_type
    -> (
      (* In the case where our type is truly recursive, we need to instead do `type_aux ()` *)
      let eq (ct1 : core_type) (ct2 : core_type) =
        (* TODO: This is a terrible way to compare the types... *)
        string_of_core_type ct1 = string_of_core_type ct2
      in
      match CCList.assoc_opt ~eq other_type substitutions with
      | Some replacement -> replacement
      | None -> Ast_builder.Default.evar ~loc (to_decoder_name lid))
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
  let argn = Printf.sprintf "arg%d" in
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
    let var = argn i in
    let var_pat = Ast_builder.Default.pvar ~loc var in
    ( (fun body ->
        partial_expr
          [%expr [%e next_decoder] >>=:: fun [%p var_pat] -> [%e body]]),
      i + 1 )
  in
  let complete_partial_expr, var_count =
    List.fold_left fn_builder base typ_decoder_exprs
  in
  let var_names = CCList.init var_count argn in
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
  if pcd_args = Pcstr_tuple [] then
    let cstr = lident_of_constructor_decl cstr_decl in
    let cstr = Ast_builder.Default.pexp_construct ~loc cstr None in
    [%expr succeed [%e cstr]]
  else
    let cstr = lident_of_constructor_decl cstr_decl in
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

let implementation_generator ~(loc : location) ~rec_flag ~substitutions
    type_decl : expression =
  let rec_flag = really_recursive rec_flag [ type_decl ] in
  let name = to_decoder_name type_decl.ptype_name.txt in
  (* let substitutions = *)
  (*   match rec_flag with *)
  (*   | Nonrecursive -> [] *)
  (*   | Recursive -> *)
  (*       [ *)
  (*         ( core_type_of_type_declaration type_decl, *)
  (*           Ast_builder.Default.evar ~loc *)
  (*             (to_decoder_name type_decl.ptype_name.txt ^ "_aux") ); *)
  (*       ] *)
  (* in *)
  let imple_expr =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest -> expr_of_typ ~substitutions manifest
    | Ptype_variant cstrs, None ->
        let constr_decs =
          Ast_builder.Default.(
            elist ~loc
              (List.map
                 (fun cstr ->
                   let s = estring ~loc cstr.pcd_name.txt in
                   let s_p = pstring ~loc cstr.pcd_name.txt in
                   if cstr.pcd_args = Pcstr_tuple [] then
                     let lid = lident_of_constructor_decl cstr in
                     let cstr =
                       Ast_builder.Default.pexp_construct ~loc lid None
                     in
                     pexp_tuple ~loc
                       [
                         s;
                         [%expr
                           D.string >>= function
                           | [%p s_p] -> succeed [%e cstr]
                           | _ -> fail "Failure"];
                         (* TODO better failure message *)
                       ]
                   else
                     pexp_tuple ~loc
                       [
                         s;
                         [%expr
                           D.field [%e s]
                             [%e expr_of_constr_decl ~substitutions cstr]];
                       ])
                 cstrs))
        in
        let one_of_decoder = Ast_builder.Default.evar ~loc "one_of" in
        let full_dec =
          Ast_helper.Exp.apply ~loc one_of_decoder [ (Nolabel, constr_decs) ]
        in
        [%expr
          let open D in
          [%e full_dec]]
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
  let name = to_decoder_name type_decl.ptype_name.txt in
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
  let name = to_decoder_name type_decl.ptype_name.txt in
  [%str let [%p Ast_builder.Default.pvar ~loc name] = [%e imple]]

let rec mutual_rec_fun_pats_gen ~loc (type_decls : type_declaration list) =
  let open Ast_builder.Default in
  match type_decls with
  | [ type_decl ] ->
      let dec_name =
        evar ~loc:type_decl.ptype_name.loc type_decl.ptype_name.txt
      in
      let auto_arg =
        (Nolabel, evar ~loc:type_decl.ptype_name.loc type_decl.ptype_name.txt)
      in
      [ pexp_apply ~loc dec_name [ auto_arg ] ]
  | type_decl :: rest ->
      let dec_name =
        evar ~loc:type_decl.ptype_name.loc type_decl.ptype_name.txt
      in
      let args =
        List.map
          (fun type_decl ->
            ( Nolabel,
              evar ~loc:type_decl.ptype_name.loc type_decl.ptype_name.txt ))
          rest
      in
      let dec_func_pattern = pexp_apply ~loc dec_name args in
      dec_func_pattern :: mutual_rec_fun_pats_gen ~loc rest
  | [] -> []

let rec mutual_rec_fun_imples_gen ~loc ~substitutions
    (type_decls : type_declaration list) =
  let open Ast_builder.Default in
  match type_decls with
  | type_decl :: rest ->
      let imple =
        implementation_generator ~loc ~rec_flag:Recursive ~substitutions
          type_decl
      in
      let substi =
        pexp_apply ~loc
          (evar ~loc (to_decoder_name type_decl.ptype_name.txt))
          (List.map
             (fun decl ->
               (Nolabel, evar ~loc (to_decoder_name decl.ptype_name.txt)))
             rest)
      in
      let new_substitution =
        (core_type_of_type_declaration type_decl, substi)
      in
      let substitutions = new_substitution :: substitutions in
      imple :: mutual_rec_fun_imples_gen ~loc ~substitutions rest
  | [] -> []

let mutual_rec_types_decoders_gen ~(loc : location) type_decls =
  let fun_patters = mutual_rec_fun_pats_gen ~loc type_decls in
  let fun_imples =
    mutual_rec_fun_imples_gen ~loc ~substitutions:[] type_decls
  in
  let fun_blocks = CCList.combine fun_patters fun_imples in
  CCList.map
    (fun pattern imple -> [%str let [%p pattern] = [%e imple]])
    fun_blocks

let str_gens ~(loc : location) ~(path : label)
    ((rec_flag : rec_flag), type_decls) : structure_item list =
  let _path = path in
  match (really_recursive rec_flag type_decls, type_decls) with
  | Nonrecursive, _ ->
      List.(flatten (map (single_type_decoder_gen ~loc ~rec_flag) type_decls))
  | Recursive, [ type_decl ] -> single_type_decoder_gen ~loc ~rec_flag type_decl
  | Recursive, _type_decls -> mutual_rec_types_decoders_gen ~loc type_decls
