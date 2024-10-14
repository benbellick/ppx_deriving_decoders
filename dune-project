(lang dune 3.11)

(name ppx_deriving_decoders)

(generate_opam_files true)

(source
 (github benbellick/ppx_deriving_decoders))

(authors "Ben Bellick")

(maintainers "Ben Bellick")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ppx_deriving_decoders)
 (synopsis "Deriving Decoders using PPX")
 (description "A longer description")
 (depends
   ocaml
   dune
   ppxlib
   decoders
   containers
  (decoders-yojson :with-test)
  (inline_tests :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
