# ppx_deriving_decoders: Automatically write mattjbray/ocaml-decoders

mattjbray/ocaml-decoders is an excellent library for writing decoders using decoding combinators. However, writing out decoders by hand for more complicated types can be quite time-intensive. 

This library helps by automatically producing the appropriate decoder for a particular type. 

There are two primary ways in which this library can be of use. (More details of both follows.)

1. "I want to write a (e.g. JSON) decoder for a particular type but don't care about the details" --> You can then use this library via `[@@deriving decoders]` applied to your types. 
2. "I want to write a (e.g. JSON) decoder for a particular type, but I care a lot about how it works and just want a good starting place" --> You can use this library via `[@@deriving_inline decoders]` applied to your types to generate the implementation in place.


## Getting Started

```
opam install ppx_deriving_decoders
```

The implementation is agnostic to the underlying decoders backend. The only requirement is the presence of a module with the signature `Decoders.Decoder.S` as specified in mattjbray/ocaml-decoders, which is aliased to module `D`.

E.g., if you wanted to decode using `yojson`, you could use 
```
opam install decoders-yojson
```

## Just generate the decoder for me

Suppose we have the following file: 

```ocaml
(* In file foo.ml *)

type bar = Int of int | String of string
```

To generate a decoder for `bar`, first add the preprocessing deriective to the appropriate dune file: 
```lisp
 (preprocess (pps ppx_deriving_decoders))
```

Then just add an implementor of `Decoders.Decode.S` to the file, aliased to `D`, and add the deriving extension:
```ocaml
(* In file foo.ml *)
module D = Decoders_yojson.Safe.Decode

type bar = Int of int | String of string [@@deriving decoders]
```

After doing this, you will have available in this module a value `bar_decoder` of type `bar D.decoder`. Then you'll be able to use this decoder freely, e.g.:
```ocaml
let () = assert (
  match D.decode_string my_basic_cstr_decoder {|{"Int": [10]}|} with
  | Ok b -> b = Int 10
  | Error _ -> false
)
```

## Only get the decoder started for me
Suppose we have the same file again:
```ocaml
(* In file foo.ml *)

type bar = Int of int | String of string
```
To generate a decoder for `bar`, we again first add the preprocessing deriective to the appropriate dune file: 
```lisp
 (preprocess (pps ppx_deriving_decoders))
```
We change the file to be
```ocaml
(* In file foo.ml *)
module D = Decoders_yojson.Safe.Decode

type bar = Int of int | String of string [@@deriving_inline decoders]

[@@@deriving.end]
```

Then, after running `dune build --auto-promote`, our file will become:
```ocaml
(* In file foo.ml *)
module D = Decoders_yojson.Safe.Decode

type bar = Int of int | String of string [@@deriving_inline decoders]

let _ = fun (_ : bar) -> ()
let bar_decoder =
  let open D in
    one_of
      [("Int",
         (D.field "Int"
            (let open D in
               let (>>=::) fst rest = uncons rest fst in
               D.int >>=:: (fun arg0 -> succeed (Int arg0)))));
      ("String",
        (D.field "String"
           (let open D in
              let (>>=::) fst rest = uncons rest fst in
              D.string >>=:: (fun arg0 -> succeed (String arg0)))))]
let _ = bar_decoder
[@@@deriving.end]
```

You can now freely remove the deriving attributes, and edit the decoder as you see fit!

## More complicated example
The following file:
```ocaml
(* In file foo.ml *)
module D = Decoders_yojson.Safe.Decode

type expr = Num of int | BinOp of op * expr * expr

and op = Add | Sub | Mul | Div

[@@@deriving.end]
```
after invoking `dune build --auto-promote` will yield:
```ocaml 
(* In file foo.ml *)
module D = Decoders_yojson.Safe.Decode

type expr = Num of int | BinOp of op * expr * expr

and op = Add | Sub | Mul | Div
[@@deriving decoders] [@@deriving_inline decoders]

let _ = fun (_ : expr) -> ()
let _ = fun (_ : op) -> ()
[@@@ocaml.warning "-27"]
let expr_decoder op_decoder =
  D.fix
    (fun expr_decoder_aux ->
       let open D in
         one_of
           [("Num",
              (D.field "Num"
                 (let open D in
                    let (>>=::) fst rest = uncons rest fst in
                    D.int >>=:: (fun arg0 -> succeed (Num arg0)))));
           ("BinOp",
             (D.field "BinOp"
                (let open D in
                   let (>>=::) fst rest = uncons rest fst in
                   op_decoder >>=::
                     (fun arg0 ->
                        expr_decoder_aux >>=::
                          (fun arg1 ->
                             expr_decoder_aux >>=::
                               (fun arg2 ->
                                  succeed (BinOp (arg0, arg1, arg2))))))))])
let _ = expr_decoder
let op_decoder op_decoder =
  let open D in
    one_of
      [("Add",
         (D.string >>=
            ((function | "Add" -> succeed Add | _ -> fail "Failure"))));
      ("Sub",
        (D.string >>=
           ((function | "Sub" -> succeed Sub | _ -> fail "Failure"))));
      ("Mul",
        (D.string >>=
           ((function | "Mul" -> succeed Mul | _ -> fail "Failure"))));
      ("Div",
        (D.string >>=
           ((function | "Div" -> succeed Div | _ -> fail "Failure"))))]
let _ = op_decoder
let op_decoder = D.fix op_decoder
let _ = op_decoder
let expr_decoder = expr_decoder op_decoder
let _ = expr_decoder
[@@@ocaml.warning "+27"]
[@@@deriving.end]
```
Notice that the mutual recursion is handled for you!

## Limitations
- Some of the decoders can be quite complicated relative to what you would write by hand
- There is not great support for types which feature type variables
- There are a lot of rough edges in places like: 
  - Error reporting
  - Correctly handling `loc`

## Future Work
- [ ] Automatically generate corresponding encoders which are inverses of the decoders
- [ ] Better handling of type variables 
- [ ] Simplify generated decoders
- [ ] Generate decoders from a module

## Contributing

Contributions are always welcome. Please create an issue as appropriate, and open a PR into the `main` branch and I'll have a look :) 
