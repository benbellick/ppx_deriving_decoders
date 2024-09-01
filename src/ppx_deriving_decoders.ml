open Ppxlib
module D = Decoders_yojson.Safe.Decode

let str_gen = Expander.str_gen
let name = "decoders"

let with_suffix_expr ~loc s =
  let dynamic_node = Ast_builder.Default.estring ~loc s in
  [%expr [%e dynamic_node] ^ "some_fixed_suffix"]

let () =
  let str_type_decl = Deriving.Generator.make_noarg str_gen in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add name ~str_type_decl (* ~sig_type_decl *) |> Deriving.ignore
