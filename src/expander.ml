open Ppxlib
(* open Ast_helper *)

(* open Asttypes *)
module D = Decoders_yojson.Safe.Decode

let expr_of_typ ~loc (typ : core_type) : expression =
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
  | _ -> failwith "Unhandled"
(* | [%type: bool] -> format "%B" *)
(* | [%type: char] -> format "%C" *)
(* | [%type: string] | [%type: String.t] -> format "%S" *)
(* | { ptyp_desc = Ptyp_any; _ } *)
(* | { ptyp_desc = Ptyp_var _; _ } *)
(* | { ptyp_desc = Ptyp_arrow (_, _, _); _ } *)
(* | { ptyp_desc = Ptyp_tuple _; _ } *)
(* | { ptyp_desc = Ptyp_constr (_, _); _ } *)
(* | { ptyp_desc = Ptyp_object (_, _); _ } *)
(* | { ptyp_desc = Ptyp_class (_, _); _ } *)
(* | { ptyp_desc = Ptyp_alias (_, _); _ } *)
(* | { ptyp_desc = Ptyp_variant (_, _, _); _ } *)
(* | { ptyp_desc = Ptyp_poly (_, _); _ } *)
(* | { ptyp_desc = Ptyp_package _; _ } *)
(* | { ptyp_desc = Ptyp_extension _; _ } -> *)

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
