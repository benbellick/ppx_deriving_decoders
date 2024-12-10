open Ppxlib

let generate_attribute v ~loc =
  let open Ast_builder.Default in
  pstr_attribute ~loc
    (attribute ~loc
       ~name:(Located.mk ~loc "ocaml.warning")
       ~payload:(PStr [ pstr_eval ~loc (estring ~loc v) [] ]))

let suppress_warning_27 ~loc = generate_attribute ~loc "-27"
let enforce_warning_27 ~loc = generate_attribute ~loc "+27"

let wrap_27 xs =
  (suppress_warning_27 ~loc:Location.none :: xs)
  @ [ enforce_warning_27 ~loc:Location.none ]

let lident_of_constructor_decl (cd : constructor_declaration) =
  let loc = cd.pcd_name.loc in
  let name = cd.pcd_name.txt in
  Ast_builder.Default.Located.lident ~loc name

let argn = Printf.sprintf "arg%d"
