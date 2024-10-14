(lang dune 3.11)

(name ppx_deriving_decoders)

(generate_opam_files true)

(source
 (github benbellick/ppx_deriving_decoders))

(authors "Ben Bellick")

(maintainers "Ben Bellick")

(license MIT)

(documentation https://github.com/benbellick/ppx_deriving_decoders)

(package
 (name ppx_deriving_decoders)
 (synopsis "Deriving Decoders using PPX")
 (description "Using mattjbray/ocaml-decoders, use a ppx to automatically \
              generate instances of a decoder for a particular type using PPX.")
 (depends
   ocaml
   dune
   ppxlib
   decoders
   containers
  (decoders-yojson :with-test)
  (inline_tests :with-test))
 (tags
  (topics "decoders" "decoding" "json" "ppx")))

