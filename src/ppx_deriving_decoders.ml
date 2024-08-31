open Ppxlib

let str_gen ~(loc : location) ~(path : label)
    ((_rec : rec_flag), (t : type_declaration list)) : structure_item list =
  let _loc = loc in
  let _path = path in
  let t = List.hd t in
  match t.ptype_kind with
  | Ptype_abstract -> []
  | Ptype_variant _ -> []
  | Ptype_record _ -> []
  | Ptype_open -> []

(* let str_gen ~loc ~path (_rec, (t : type_declaration)) = *)

(* let sig_gen ~loc ~path:_ (_rec, t) = *)
(*   let (module Ast) = Ast_builder.make loc in *)
(*   (\* we are silently dropping mutually recursive definitions to keep things *)
(*     brief *\) *)
(*   let t = List.hd t in *)
(*   let name = module_name_of_type t in *)
(*   let type_ = *)
(*     let sig_ = *)
(*       [%sig: *)
(*         val path : string *)
(*         val name : string *)
(*       ] *)
(*     in *)
(*     Ast.pmty_signature sig_ *)
(*   in *)
(*   Ast.module_declaration ~name ~type_ *)
(*   |> Ast.psig_module *)
(*   |> fun x -> [ x ] *)

let name = "decoders"

let () =
  let str_type_decl = Deriving.Generator.make_noarg str_gen in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add name ~str_type_decl (* ~sig_type_decl *) |> Deriving.ignore
