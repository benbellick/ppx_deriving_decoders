open Ppxlib
module D = Decoders_yojson.Safe.Decode

let to_decoder_name i = i ^ "_decoder"

let lident_of_constructor_decl (cd : constructor_declaration) =
  let loc = cd.pcd_name.loc in
  let name = cd.pcd_name.txt in
  Ast_builder.Default.Located.lident ~loc name (* Convert to lident *)

let rec expr_of_typ (typ : core_type) : expression =
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
      let sub_expr = expr_of_typ inner_typ in
      Ast_helper.Exp.apply ~loc list_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] array] ->
      let array_decoder = Ast_builder.Default.evar ~loc "D.array" in
      let sub_expr = expr_of_typ inner_typ in
      Ast_helper.Exp.apply ~loc array_decoder [ (Nolabel, sub_expr) ]
  | [%type: [%t? inner_typ] option] ->
      let opt_decoder = Ast_builder.Default.evar ~loc "D.nullable" in
      let sub_expr = expr_of_typ inner_typ in
      Ast_helper.Exp.apply ~loc opt_decoder [ (Nolabel, sub_expr) ]
  | { ptyp_desc = Ptyp_tuple typs; _ } -> expr_of_tuple ~loc typs
  (* | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } -> _ *)
  | { ptyp_desc = Ptyp_alias _; _ } ->
      failwith
        (Format.sprintf "This alias was a failure...: %s\n"
           (string_of_core_type typ))
  | {
   ptyp_desc =
     Ptyp_constr
       ({ txt = Lident lid (* TODO Do we need to cover other cases? *); _ }, []);
   _;
  } ->
      Ast_builder.Default.evar ~loc (to_decoder_name lid)
  | _ ->
      Location.raise_errorf ~loc "Cannot construct decoder for %s"
        (string_of_core_type typ)

and expr_of_tuple ~loc ?lift typs =
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
  let typ_decoder_exprs = List.map expr_of_typ typs in
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

and expr_of_constr_decl
    ({ pcd_name; pcd_args; pcd_loc = loc; _ } as cstr_decl :
      constructor_declaration) =
  if pcd_args = Pcstr_tuple [] then
    let s_exp = Ast_builder.Default.estring ~loc pcd_name.txt in
    let s_pat = Ast_builder.Default.pstring ~loc pcd_name.txt in
    let cstr = lident_of_constructor_decl cstr_decl in
    let cstr = Ast_builder.Default.pexp_construct ~loc cstr None in
    [%expr
      let open D in
      let open D.Infix in
      D.string >>= function
      | [%p s_pat] -> succeed [%e cstr]
      | e -> fail ("Could not decode " ^ e ^ " into " ^ [%e s_exp])]
  else
    let field_decoder = Ast_builder.Default.evar ~loc "D.field" in
    let field = Ast_builder.Default.estring ~loc pcd_name.txt in
    let cstr = lident_of_constructor_decl cstr_decl in
    let sub_expr = expr_of_constr_arg ~loc ~cstr pcd_args in
    Ast_helper.Exp.apply ~loc field_decoder
      [ (Nolabel, field); (Nolabel, sub_expr) ]

and expr_of_constr_arg ~loc ~cstr (arg : constructor_arguments) =
  match arg with
  | Pcstr_tuple tups -> expr_of_tuple ~lift:cstr ~loc tups
  | Pcstr_record _ ->
      Location.raise_errorf ~loc "Unhandled record in constr decl arg"

and expr_of_record ~loc label_decls =
  (* To help understand what this function is doing, imagine we had
     a type [type t = {i : int; s : string}]. Then this will render the decoder:
     let t_decoder : t D.decoder =
     let open D in
     let open D.Infix in
     let* i = field "i" int in
     let* s = field "s" string in
     succeed {i; s}
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
    let subexpr = expr_of_typ pld_type in
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
  complete_partial_expr [%expr succeed [%e record]]

let str_gen ~(loc : location) ~(path : label) ((rec_flag : rec_flag), type_decls)
    : structure_item list =
  let rec_flag = really_recursive rec_flag type_decls in
  let _path = path in
  let type_decl = List.hd type_decls in
  let name = to_decoder_name type_decl.ptype_name.txt in
  print_string "\n\n Some INFO: \n";
  print_string "really recursive? ";
  print_string (match rec_flag with Recursive -> "Yes" | Nonrecursive -> "No");
  print_string "\n type: ";
  print_string type_decl.ptype_name.txt;
  match (type_decl.ptype_kind, type_decl.ptype_manifest) with
  | Ptype_abstract, Some manifest ->
      [%str
        let [%p Ast_builder.Default.pvar ~loc name] = [%e expr_of_typ manifest]]
  | Ptype_variant cstrs, None ->
      let constr_decs =
        Ast_builder.Default.(
          elist ~loc
            (List.map
               (fun cstr ->
                 pexp_tuple ~loc
                   [ estring ~loc cstr.pcd_name.txt; expr_of_constr_decl cstr ])
               cstrs))
      in
      let one_of_decoder = Ast_builder.Default.evar ~loc "D.one_of" in
      let app =
        Ast_helper.Exp.apply ~loc one_of_decoder [ (Nolabel, constr_decs) ]
      in
      [%str let [%p Ast_builder.Default.pvar ~loc name] = [%e app]]
  | Ptype_record label_decs, _ ->
      [%str
        let [%p Ast_builder.Default.pvar ~loc name] =
          [%e expr_of_record ~loc label_decs]]
  | Ptype_open, _ -> Location.raise_errorf ~loc "Unhandled open"
  | _ -> Location.raise_errorf ~loc "Unhandled mystery"
