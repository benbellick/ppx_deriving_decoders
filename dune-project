(lang dune 3.11)

(name ppx_deriving_decoders)

(generate_opam_files true)

(version 0.1)

(source
 (github benbellick/ppx_deriving_decoders))

(authors "Ben Bellick <benbellick@pm.me>")

(maintainers "Ben Bellick <benbellick@pm.me>")

(license MIT)

(documentation https://github.com/benbellick/ppx_deriving_decoders)

(package
 (name ppx_deriving_decoders)
 (synopsis "Deriving Decoders using PPX")
 (description "Using mattjbray/ocaml-decoders, use a ppx to automatically \
              generate instances of a decoder for a particular type using PPX.")
 (depends
  (ocaml (>= 4.08.0))
   dune
  (ppxlib (>= 0.20.0))
  (decoders (>= 0.5.0))
  (containers (>= 2.8))
  (decoders-yojson :with-test)
  (ppx_inline_test :with-test))
 (tags
  ("decoders" "decoding" "json" "ppx")))

