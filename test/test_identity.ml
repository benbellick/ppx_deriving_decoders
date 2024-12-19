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

module ComplexRec = struct
  type a = { l : b option; m : c option }
  and b = { n : c }
  and c = { o : a } [@@deriving decoders, encoders]

  module OnJson = struct
    open OnJson

    let id = make_id a_encoder a_decoder
    let check = check id
    let%test "a_rec:json:1" = check {|{"l":null,"m":null}|}
  end

  module OnValue = struct
    open OnValue

    let id = make_id a_encoder a_decoder
    let check = check id
    let%test "a1:value:1" = check { l = None; m = None }

    let%test "a1:value:2" =
      check
        {
          l =
            Some
              {
                n =
                  { o = { l = None; m = Some { o = { l = None; m = None } } } };
              };
          m = None;
        }
  end
end

module Nesting = struct
  module Outer = struct
    type ('a, 'b, 'c) t = Fst of 'a list | Snd of 'b option | Trd of 'c
    [@@deriving decoders, encoders]

    module Inner = struct
      type t = int [@@deriving decoders, encoders]
    end
  end

  type t = (string, Outer.Inner.t, bool) Outer.t [@@deriving decoders, encoders]

  module OnJson = struct
    open OnJson

    let id = make_id t_encoder t_decoder
    let check = check id

    let%test "nesting:value:1" =
      check {|{"Fst":["okay","now","we","have","a","string"]}|}

    let%test "nesting:value:2" = check {|{"Snd":21092}|}
    let%test "nesting:value:3" = check {|{"Trd":false}|}
  end

  module OnValue = struct
    open OnValue

    let id = make_id t_encoder t_decoder
    let check = check id
    let%test "nesting:value:1" = check (Fst [ "there"; "is"; "some"; "string" ])
    let%test "nesting:value:2" = check (Snd (Some 10))
    let%test "nesting:value:3" = check (Trd false)
  end
end
