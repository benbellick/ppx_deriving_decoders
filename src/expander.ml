open Ppxlib
module D = Decoders_yojson.Safe.Decode

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
  | _ -> failwith "Unhandled"

and expr_of_tuple ~loc = function
  | [] -> failwith "Cannot have an empty tuple"
  | typ :: typs ->
      let base = expr_of_typ typ in
      let uncons = Ast_builder.Default.evar ~loc "D.uncons" in
      let pipe = Ast_builder.Default.evar ~loc "(|>)" in
      (* TODO this location cannot be correct *)
      let ( |> ) v f =
        (* TODO this location cannot be correct *)
        Ast_builder.Default.eapply ~loc pipe [ v; f ]
      in
      let folder tuple_dec ty : expression =
        let ty_dec = expr_of_typ ty in
        let uncons_f = Ast_builder.Default.eapply ~loc uncons [ tuple_dec ] in
        ty_dec |> uncons_f
      in
      List.fold_left folder base typs

let str_gen ~(loc : location) ~(path : label)
    ((_rec : rec_flag), (type_decl : type_declaration list)) :
    structure_item list =
  let _path = path in
  let _loc = loc in
  let type_decl = List.hd type_decl in
  let name = type_decl.ptype_name.txt in
  match (type_decl.ptype_kind, type_decl.ptype_manifest) with
  | Ptype_abstract, Some manifest ->
      [%str
        let [%p Ast_builder.Default.pvar ~loc (name ^ "_decoder")] =
          [%e expr_of_typ manifest]]
  | Ptype_variant _v, _ -> []
  | Ptype_record _, _ -> []
  | Ptype_open, _ -> []
  | _ -> [] (* TODO handle other cases *)
