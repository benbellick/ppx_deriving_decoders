open Ppxlib

let () =
  let name = "decoders" in
  let str_type_decl = Deriving.Generator.make_noarg Decoders_deriver.str_gens in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add name ~str_type_decl (* ~sig_type_decl *) |> Deriving.ignore

let () =
  let name = "encoders" in
  let str_type_decl = Deriving.Generator.make_noarg Encoders_deriver.str_gens in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add name ~str_type_decl (* ~sig_type_decl *) |> Deriving.ignore
