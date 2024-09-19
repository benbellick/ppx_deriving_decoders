open Ppxlib
module D = Decoders_yojson.Safe.Decode

let name = "decoders"

let () =
  let str_type_decl = Deriving.Generator.make_noarg Expander.str_gens in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add name ~str_type_decl (* ~sig_type_decl *) |> Deriving.ignore
