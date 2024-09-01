open Ppxlib
module D = Decoders_yojson.Safe.Decode

let str_gen = Expander.str_gen
let name = "decoders"

let () =
  let str_type_decl = Deriving.Generator.make_noarg str_gen in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add name ~str_type_decl (* ~sig_type_decl *) |> Deriving.ignore
