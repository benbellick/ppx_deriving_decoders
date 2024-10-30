dumpast:
	ocamlfind ppx_tools/dumpast $(file)

opam-install-dev-deps:
	opam install ocamlformat ocaml-lsp-server ppx_tools

show-ppx-test:
	dune exec -- pp/pp.exe	test/test.ml
