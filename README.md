# ppx_deriving_decoders: Automatically write mattjbray/ocaml-decoders

[mattjbray/ocaml-decoders](https://github.com/mattjbray/ocaml-decoders) is an excellent library for writing decoders using decoding combinators. However, writing out decoders by hand for more complicated types can be quite time-intensive. 
  
This library helps by automatically producing the appropriate decoder (and encoder!) for a particular type. 

There are two primary ways in which this library can be of use. (More details of both follows.)

1. "I want to write a (e.g. JSON) decoder for a particular type but don't care about the details" --> You can then use this library via `[@@deriving decoders]` applied to your types. 
2. "I want to write a (e.g. JSON) decoder for a particular type, but I care a lot about how it works and just want a good starting place" --> You can use this library via `[@@deriving_inline decoders]` applied to your types to generate the implementation in place.

> [!WARNING]
> This is still a fairly experimental library. Use at your own risk! If you would like to use it and be extra safe, use approach 2 above, by using the library to generate boilerplate and then removing the dependency in production code. 

## Getting Started

```
opam install ppx_deriving_decoders
```

The implementation is agnostic to the underlying decoders back-end. The only requirement is the presence of a module with the signature [`Decoders.Decode.S`](https://github.com/mattjbray/ocaml-decoders/blob/59c0dfbe6026af27fce96af82e650a875157385d/src/sig.ml#L8) as specified in [mattjbray/ocaml-decoders](https://github.com/mattjbray/ocaml-decoders), which is aliased to module `D`.

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

To generate a decoder for `bar`, first add the preprocessing directive to the appropriate dune file: 
```lisp
 (preprocess (pps ppx_deriving_decoders))
```

Then just add an implementer of `Decoders.Decode.S` to the file, aliased to `D`, and add the deriving extension:
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
To generate a decoder for `bar`, we again first add the preprocessing directive to the appropriate dune file: 
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

Then, after running `dune build --auto-promote`, our file will become (after applying `ocamlformat`):
```ocaml
(* In file foo.ml *)
module D = Decoders_yojson.Safe.Decode

type bar = Int of int | String of string [@@deriving_inline decoders]
let _ = fun (_ : bar) -> ()

let bar_decoder =
  let open D in
  single_field (function
    | "Int" -> D.int >|= fun arg -> Int arg
    | "String" -> D.string >|= fun arg -> String arg
    | any -> D.fail @@ Printf.sprintf "Unrecognized field: %s" any)

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
and op = Add | Sub | Mul | Div [@@deriving_inline decoders]

[@@@deriving.end]
```
after invoking `dune build --auto-promote` (plus `ocamlformat`) will yield:
```ocaml 
(* In file foo.ml *)
 type expr = Num of int | BinOp of op * expr * expr
 and op = Add | Sub | Mul | Div [@@deriving_inline decoders]

let _ = fun (_ : expr) -> ()
let _ = fun (_ : op) -> ()

[@@@ocaml.warning "-27"]

let expr_decoder op_decoder =
  D.fix (fun expr_decoder_aux ->
      let open D in
      single_field (function
        | "Num" -> D.int >|= fun arg -> Num arg
        | "BinOp" ->
            let open D in
            let ( >>=:: ) fst rest = uncons rest fst in
            op_decoder >>=:: fun arg0 ->
            expr_decoder_aux >>=:: fun arg1 ->
            expr_decoder_aux >>=:: fun arg2 ->
            succeed (BinOp (arg0, arg1, arg2))
        | any -> D.fail @@ Printf.sprintf "Unrecognized field: %s" any))

let _ = expr_decoder

let op_decoder op_decoder =
  let open D in
  single_field (function
    | "Add" -> succeed Add
    | "Sub" -> succeed Sub
    | "Mul" -> succeed Mul
    | "Div" -> succeed Div
    | any -> D.fail @@ Printf.sprintf "Unrecognized field: %s" any)

let _ = op_decoder
let op_decoder = D.fix op_decoder
let _ = op_decoder
let expr_decoder = expr_decoder op_decoder
let _ = expr_decoder

[@@@ocaml.warning "+27"]

[@@@deriving.end]
```
Notice that the mutual recursion is handled for you!

## Type vars
The `ppx` can also handle types with type variables: 
```ocaml
type 'a wrapper = { wrapped : 'a } [@@deriving_inline decoders]
[@@@deriving.end]
```
becomes (additionally with `ocamlformat`): 

```ocaml
type 'a record_wrapper = { wrapped : 'a } [@@deriving_inline decoders]

let _ = fun (_ : 'a record_wrapper) -> ()

let record_wrapper_decoder a_decoder =
  let open D in
  let open D.Infix in
  let* wrapped = field "wrapped" a_decoder in
  succeed { wrapped }

let _ = record_wrapper_decoder

[@@@deriving.end]
```
Notice that the decoder for the type variable becomes a parameter of the generated decoder!

## Encoders
All of the above information also applies to generating encoders. Using the above type as an example: 
```ocaml
type 'a wrapper = { wrapped : 'a } [@@deriving_inline decoders]
[@@@deriving.end]
```
becomes (additionally with `ocamlformat`): 

```ocaml
type 'a wrapper = { wrapped : 'a } [@@deriving_inline encoders]

let _ = fun (_ : 'a record_wrapper) -> ()

let wrapper_encoder a_encoder { wrapped } =
  E.obj [ ("wrapped", a_encoder wrapped) ]

let _ = record_wrapper_encoder

[@@@deriving.end]
```

Of course, you can generate both by using `[@@deriving_inline decoders, encoders]` or `[@@deriving decoders, encoders]`. The corresponding pair will be inverses of one another provided that all prior referenced decoder/encoder pairs are inverses!


## Limitations
- Some of the decoders can be quite complicated relative to what you would write by hand
- There are a lot of rough edges in places like: 
  - Error reporting
  - Correctly handling `loc`
- In an ideal world, it would be nice to generate the corresponding decoders/encoders within their own submodule. It remains to be seen how this can be done. 

## Future Work
- [ ] Simplify generated decoders
- [ ] Generate decoders from a module
- [ ] How to handle types produced from functors inline

## Contributing

Contributions are always welcome. Please create an issue as appropriate, and open a PR into the `main` branch and I'll have a look :) 
