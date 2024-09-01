open Ppxlib
module D = Decoders_yojson.Safe.Decode

let str_gen ~(loc : location) ~(path : label)
    ((_rec : rec_flag), (t : type_declaration list)) : structure_item list =
  let _loc = loc in
  let _path = path in
  let t = List.hd t in
  match t.ptype_kind with
  | Ptype_abstract -> (
      let name = t.ptype_name.txt in
      (* let fn_name = name ^^ "of_yojson" in *)
      let _cstr = t.ptype_cstrs in
      let core_type_desc =
        (* TODO: shouldn't be using this function *)
        (CCOption.get_exn_or "Error getting ptype_manifest" t.ptype_manifest)
          .ptyp_desc
      in
      match core_type_desc with
      | Ptyp_constr ({ txt = Lident "int"; loc }, _) ->
          [%str
            let [%p Ast_builder.Default.pvar ~loc (name ^ "_decoder")] = D.int]
      | _ -> [] (* TODO should handle rest *))
  | Ptype_variant _v -> []
  | Ptype_record _ -> []
  | Ptype_open -> []
