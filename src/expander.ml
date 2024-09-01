open Ppxlib
(* open Ast_helper *)

(* open Asttypes *)
module D = Decoders_yojson.Safe.Decode

let my_func x y = x + y

let rec expr_of_typ ~loc (typ : core_type) : expression =
  match typ with
  (* | [%type: unit] | [%type: unit] -> *)
  (* [%expr fun () -> Ppx_deriving_runtime.Format.pp_print_string fmt "()"] *)
  | [%type: int]
  | [%type: int32]
  | [%type: Int32.t]
  | [%type: int64]
  | [%type: Int64.t]
  | [%type: nativeint]
  | [%type: Nativeint.t] ->
      Ast_builder.Default.evar ~loc "D.int"
  | [%type: float] -> Ast_builder.Default.evar ~loc "D.float"
  | [%type: bool] -> Ast_builder.Default.evar ~loc "D.bool"
  | [%type: char] | [%type: string] | [%type: String.t] ->
      Ast_builder.Default.evar ~loc "D.char"
  | [%type: bytes] | [%type: Bytes.t] ->
      failwith "Cannot handle Bytes" (* TODO: figure out strategy *)
  | [%type: [%t? inner_typ] list] | [%type: [%t? inner_typ] array] ->
      let list_decoder = Ast_builder.Default.evar ~loc "D.list" in
      let sub_expr = expr_of_typ ~loc inner_typ in
      Ast_helper.Exp.apply ~loc list_decoder [ (Nolabel, sub_expr) ]
  (* | [%type: [%t? typ] option] -> _ *)
  | _ -> failwith "Unhandled"

let str_gen ~(loc : location) ~(path : label)
    ((_rec : rec_flag), (type_decl : type_declaration list)) :
    structure_item list =
  let _path = path in
  Format.print_string "This is the string: ";
  Format.print_string path;
  let type_decl = List.hd type_decl in
  let name = type_decl.ptype_name.txt in
  match (type_decl.ptype_kind, type_decl.ptype_manifest) with
  | Ptype_abstract, Some manifest ->
      [%str
        let [%p Ast_builder.Default.pvar ~loc (name ^ "_decoder")] =
          [%e expr_of_typ ~loc manifest]]
  | Ptype_variant _v, _ -> []
  | Ptype_record _, _ -> []
  | Ptype_open, _ -> []
  | _ -> [] (* TODO handle other cases *)
