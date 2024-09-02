open Ppxlib
module D = Decoders_yojson.Safe.Decode

let to_decoder_name i = i ^ "_decoder"

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
      (* This is the special case of e.g. type t = int, i.e. a trivial type construction *)
      (* the trick here will be the use the manifest to recursive call into this function again *)
      (* Format.sprintf "This was a failure...: %s\n" (string_of_core_type typ) *)
      Ast_builder.Default.evar ~loc (to_decoder_name lid)
  | _ ->
      failwith
        (Format.sprintf "This was a failure...: %s\n" (string_of_core_type typ))

and expr_of_tuple ~loc typs =
  (* To help understand what this function is doing, imagine we had
     a type [type t = int * string * bool]. Then this will render the decoder:
     let t_decoder : t D.decoder =
     let open D in
     let ( >>=:: ) fst rest = uncons rest fst in
     int >>=:: fun arg1 ->
     string >>=:: fun arg2 ->
     bool >>=:: fun arg3 -> succeed (arg1, arg2, arg3)
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
  complete_partial_expr [%expr succeed [%e var_tuple]]

and _expr_of_constr lid typs =
  let typ_strs = CCList.map string_of_core_type typs in
  match lid with
  | Lident txt ->
      let typf = CCFormat.(string) in
      let typ_listf = CCFormat.(list typf) in
      failwith @@ CCFormat.sprintf "%s = [@[<hov>%a@]]@." txt typ_listf typ_strs
  | _ -> failwith (Format.sprintf "Failed to decode constr")

let str_gen ~(loc : location) ~(path : label)
    ((_rec : rec_flag), (type_decl : type_declaration list)) :
    structure_item list =
  let _path = path in
  let _loc = loc in
  let type_decl = List.hd type_decl in
  let name = to_decoder_name type_decl.ptype_name.txt in
  match (type_decl.ptype_kind, type_decl.ptype_manifest) with
  | Ptype_abstract, Some manifest ->
      [%str
        let [%p Ast_builder.Default.pvar ~loc name] = [%e expr_of_typ manifest]]
  | Ptype_variant _v, _ -> []
  | Ptype_record _, _ -> []
  | Ptype_open, _ -> []
  | _ -> [] (* TODO handle other cases *)
