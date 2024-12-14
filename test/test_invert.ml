module D = Decoders_yojson.Safe.Decode
module E = Decoders_yojson.Safe.Encode

type expr =
  | Int of int
  | Real of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
[@@deriving decoders, encoders]

let make_id enc dec =
  let str_enc = E.encode_string enc in
  let str_dec = D.decode_string dec in
  CCFun.(str_dec % str_enc)

let check id v = match id v with Ok v' when v = v' -> true | _ -> false
let expr_id = make_id expr_encoder expr_decoder

let expr_id_print v =
  let str_enc = E.encode_string expr_encoder in
  let str_dec = D.decode_string expr_decoder in
  let s = str_enc v in
  print_endline s;
  str_dec s

let%test "expr_inv:1" = check expr_id (Int 10)

let%test "expr_inv:2" =
  check expr_id
    (Add
       ( Add (Int 10, Int (-5)),
         Div (Real 1.4929, Sub (Real (-5392.1239230), Int 58292349823)) ))

type my_list = Null | L of my_list [@@deriving decoders, encoders]

let my_list_id = make_id my_list_encoder my_list_decoder
let%test "my_list:1" = check my_list_id Null
let%test "my_list:2" = check my_list_id (L (L (L (L Null))))

type a_rec = { b : b_rec option }
and b_rec = { a : a_rec option } [@@deriving decoders, encoders]

let a_rec_id = make_id a_rec_encoder a_rec_decoder
let%test "a_rec:1" = check a_rec_id { b = None }
let%test "a_rec:2" = check a_rec_id { b = Some { a = None } }

let%test "a_rec:3" =
  check a_rec_id { b = Some { a = Some { b = Some { a = None } } } }

(* More complex mutual recursive type *)
type a1 = { l : b1 option; m : c1 option }
and b1 = { n : c1 }
and c1 = { o : a1 } [@@deriving decoders, encoders]

let a1_id = make_id a1_encoder a1_decoder
let%test "a1:1" = check a1_id { l = None; m = None }

let%test "a1:2" =
  check a1_id
    {
      l =
        Some
          { n = { o = { l = None; m = Some { o = { l = None; m = None } } } } };
      m = None;
    }

(* Random usage of modules intermixed with type vars *)

module Outer = struct
  type ('a, 'b, 'c) v = Fst of 'a list | Snd of 'b option | Trd of 'c
  [@@deriving decoders, encoders]

  [@@@deriving.end]

  module Inner = struct
    type t = int [@@deriving decoders, encoders]
  end
end

type nesting = (string, Outer.Inner.t, bool) Outer.v
[@@deriving decoders, encoders]

let nesting_id = make_id nesting_encoder nesting_decoder

let%test "nesting:1" =
  check nesting_id (Fst [ "there"; "is"; "some"; "string" ])

let%test "nesting:2" = check nesting_id (Snd (Some 10))
let%test "nesting:3" = check nesting_id (Trd false)
