module D = Decoders_yojson.Safe.Decode
module E = Decoders_yojson.Safe.Encode

module OnValue = struct
  let make_id enc dec =
    let str_enc = E.encode_string enc in
    let str_dec = D.decode_string dec in
    CCFun.(str_dec % str_enc)

  let check id j = match id j with Ok j' when j = j' -> true | _ -> false
end

module OnJson = struct
  let make_id (enc : 'a E.encoder) (dec : 'a D.decoder) json =
    let open CCResult in
    let str_enc = E.encode_string enc in
    let str_dec = D.decode_string dec in
    let+ value = str_dec json in
    str_enc value

  let check id v = match id v with Ok v' when v = v' -> true | _ -> false
end

module Expression = struct
  type t =
    | Int of int
    | Real of float
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
  [@@deriving decoders, encoders]

  module OnJson = struct
    open OnJson

    let id = make_id t_encoder t_decoder
    let check = check id
    let%test "expr_inv:json:1" = check {|{"Int":15}|}
    let%test "expr_inv:json:2" = check {|{"Add":[{"Int":12},{"Int":15}]}|}

    let%test "expr_inv:json:3" =
      check
        {|{"Mul":[{"Sub":[{"Real":-12043.1235},{"Int":4932}]},{"Div":[{"Int":-123},{"Real":5932.1239}]}]}|}
  end

  module OnValue = struct
    open OnValue

    let id = make_id t_encoder t_decoder
    let check = check id
    let%test "expr_inv:value:1" = check (Int 10)

    let%test "expr_inv:value:2" =
      check
        (Add
           ( Add (Int 10, Int (-5)),
             Div (Real 1.4929, Sub (Real (-5392.1239230), Int 58292349823)) ))
  end
end

module MyList = struct
  type t = Null | L of t [@@deriving decoders, encoders]

  module OnJson = struct
    open OnJson

    let id = make_id t_encoder t_decoder
    let check = check id
    let%test "my_list:json:1" = check {|{"Null":null}|}
    let%test "my_list:value:2" = check {|{"L":{"L":{"L":{"Null":null}}}}|}
  end

  module OnValue = struct
    open OnValue

    let id = make_id t_encoder t_decoder
    let check = check id
    let%test "my_list:value:1" = check Null
    let%test "my_list:value:2" = check (L (L (L (L Null))))
  end
end

module BasicRec = struct
  type a = { b : b option }
  and b = { a : a option } [@@deriving decoders, encoders]

  module OnJson = struct
    open OnJson

    let id = make_id a_encoder a_decoder
    let check = check id
    let%test "a_rec:value:1" = check {|{"b":null}|}
    let%test "a_rec:value:2" = check {|{"b":{"a":{"b":{"a":null}}}}|}
  end

  module OnValue = struct
    open OnValue

    let id = make_id a_encoder a_decoder
    let check = check id
    let%test "a_rec:value:1" = check { b = None }
    let%test "a_rec:value:2" = check { b = Some { a = None } }
  end
end

(* let a_rec_id = make_id a_rec_encoder a_rec_decoder *)
(* let%test "a_rec:1" = check a_rec_id { b = None } *)
(* let%test "a_rec:2" = check a_rec_id { b = Some { a = None } } *)

(* let%test "a_rec:3" = *)
(*   check a_rec_id { b = Some { a = Some { b = Some { a = None } } } } *)

(* (\* More complex mutual recursive type *\) *)
(* type a1 = { l : b1 option; m : c1 option } *)
(* and b1 = { n : c1 } *)
(* and c1 = { o : a1 } [@@deriving decoders, encoders] *)

(* let a1_id = make_id a1_encoder a1_decoder *)
(* let%test "a1:1" = check a1_id { l = None; m = None } *)

(* let%test "a1:2" = *)
(*   check a1_id *)
(*     { *)
(*       l = *)
(*         Some *)
(*           { n = { o = { l = None; m = Some { o = { l = None; m = None } } } } }; *)
(*       m = None; *)
(*     } *)

(* (\* Random usage of modules intermixed with type vars *\) *)

(* module Outer = struct *)
(*   type ('a, 'b, 'c) v = Fst of 'a list | Snd of 'b option | Trd of 'c *)
(*   [@@deriving decoders, encoders] *)

(*   [@@@deriving.end] *)

(*   module Inner = struct *)
(*     type t = int [@@deriving decoders, encoders] *)
(*   end *)
(* end *)

(* type nesting = (string, Outer.Inner.t, bool) Outer.v *)
(* [@@deriving decoders, encoders] *)

(* let nesting_id = make_id nesting_encoder nesting_decoder *)

(* let%test "nesting:1" = *)
(*   check nesting_id (Fst [ "there"; "is"; "some"; "string" ]) *)

(* let%test "nesting:2" = check nesting_id (Snd (Some 10)) *)
(* let%test "nesting:3" = check nesting_id (Trd false) *)
