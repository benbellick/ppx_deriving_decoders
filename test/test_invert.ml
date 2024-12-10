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

type my_list = Null | L of my_list [@@deriving decoders]

let expr_id =
  let str_enc = E.encode_string expr_encoder in
  let str_dec = D.decode_string expr_decoder in
  CCFun.(str_dec % str_enc)

let expr_id_print v =
  let str_enc = E.encode_string expr_encoder in
  let str_dec = D.decode_string expr_decoder in
  let s = str_enc v in
  print_endline s;
  str_dec s

let check id v = match id v with Ok v' when v = v' -> true | _ -> false
let%test "expr_inv1" = check expr_id (Int 10)

let%test "expr_inv2" =
  check expr_id
    (Add
       ( Add (Int 10, Int (-5)),
         Div (Real 1.4929, Sub (Real (-5392.1239230), Int 58292349823)) ))
