module D = Decoders_yojson.Safe.Decode

type my_int = int [@@deriving decoders]
type my_float = float [@@deriving decoders]
type my_string = string [@@deriving decoders]
type my_bool = bool [@@deriving decoders]
type my_int_list = int list [@@deriving decoders]
type my_int_array = int array [@@deriving decoders]
type my_unit = unit [@@deriving decoders]
type my_opt_bool = bool option [@@deriving decoders]
type my_tuple = int * string * int * bool [@@deriving decoders]
type 'a const = 'a
(* type my_int_const = int const [@@deriving decoders] *)

(* TODO: the above could become quite complex in general  *)
type my_nested_bool = my_bool [@@deriving decoders]
type my_nested_int = my_int [@@deriving decoders]
type my_deep_tuple = my_tuple * my_bool [@@deriving decoders]
type my_basic_cstr = Int of int [@@deriving decoders]
type my_basic_record = { i : int } [@@deriving decoders]

type my_complex_record = { basic : my_basic_record; cstr : my_basic_cstr }
[@@deriving decoders]

type my_basic_cstr2 = Ints of int * int | Strs of string * string
[@@deriving decoders]

type colors = Red | Blue | Green [@@deriving decoders]
type status = Online of int | Offline [@@deriving decoders]
type my_list = Null | L of my_list [@@deriving decoders]
type constr_w_rec = Empty | Item of { i : int } [@@deriving decoders]

type a_non_rec = int * string
and b_non_rec = bool [@@deriving decoders]

type a_rec = { b : b_rec option }
and b_rec = { a : a_rec option } [@@deriving decoders]

type bar = Int of int | String of string [@@deriving decoders]

type expr = Num of int | BinOp of op * expr * expr
and op = Add | Sub | Mul | Div [@@deriving decoders]

(* More complex mutual recursive type *)
type a1 = { l : b1 option; m : c1 option }
and b1 = { n : c1 }
and c1 = { o : a1 } [@@deriving decoders]

type 'a record_wrapper = { wrapped : 'a } [@@deriving decoders]
type int_record_wrapper = int record_wrapper [@@deriving decoders]

let%test "int" =
  match D.decode_string my_int_decoder "1234" with
  | Ok i -> i = 1234
  | Error _ -> false

let%test "float" =
  match D.decode_string my_float_decoder "1239001.1230" with
  | Ok f -> f = 1239001.1230
  | Error _ -> false

let%test "string" =
  match
    D.decode_string my_string_decoder {|"this is a very special string"|}
  with
  | Ok s -> s = "this is a very special string"
  | Error _ -> false

let%test "bool" =
  match D.decode_string my_bool_decoder "true" with
  | Ok s -> s
  | Error _ -> false

let%test "int list" =
  match D.decode_string my_int_list_decoder "[1, 2, 3, 4, 5]" with
  | Ok f -> f = [ 1; 2; 3; 4; 5 ]
  | Error _ -> false

let%test "int list" =
  match D.decode_string my_int_array_decoder "[1, 2, 3, 4, 5]" with
  | Ok f -> f = [| 1; 2; 3; 4; 5 |]
  | Error _ -> false

let%test "unit" =
  match D.decode_string my_unit_decoder "null" with
  | Ok f -> f = ()
  | Error _ -> false

let%test "bool option" =
  (match D.decode_string my_opt_bool_decoder "true" with
  | Ok b -> b = Some true
  | Error _ -> false)
  &&
  match D.decode_string my_opt_bool_decoder "null" with
  | Ok b -> b = None
  | Error _ -> false

let%test "tuple" =
  match D.decode_string my_tuple_decoder {|[10, "hello", 15, true]|} with
  | Ok b -> b = (10, "hello", 15, true)
  | Error _ -> false

let%test "nested bool" =
  match D.decode_string my_nested_bool_decoder "true" with
  | Ok b -> b = true
  | Error _ -> false

let%test "nested int" =
  match D.decode_string my_nested_int_decoder "10" with
  | Ok b -> b = 10
  | Error _ -> false

let%test "deep tuple" =
  match
    D.decode_string my_deep_tuple_decoder {|[[10, "hello", 15, true], true]|}
  with
  | Ok b -> b = ((10, "hello", 15, true), true)
  | Error _ -> false

let%test "basic constructor" =
  match D.decode_string my_basic_cstr_decoder {|{"Int": 10}|} with
  | Ok b -> b = Int 10
  | Error _ -> false

let%test "basic constructor 2" =
  (match D.decode_string my_basic_cstr2_decoder {|{"Ints": [10, 11]}|} with
  | Ok b -> b = Ints (10, 11)
  | Error _ -> false)
  &&
  match
    D.decode_string my_basic_cstr2_decoder {|{"Strs": ["first", "second"]}|}
  with
  | Ok b -> b = Strs ("first", "second")
  | Error _ -> false

let%test "basic record" =
  match D.decode_string my_basic_record_decoder {|{"i": 10}|} with
  | Ok b -> b = { i = 10 }
  | Error _ -> false

let%test "complex record" =
  match
    D.decode_string my_complex_record_decoder
      {|{"basic" : {"i": 10}, "cstr": {"Int": 10}}|}
  with
  | Ok b -> b = { basic = { i = 10 }; cstr = Int 10 }
  | Error _ -> false

let%test "simple constructor-less variant" =
  (match D.decode_string colors_decoder {|{"Red": {}}|} with
  | Ok Red -> true
  | _ -> false)
  &&
  match D.decode_string colors_decoder {|{"Blue": []}|} with
  | Ok Blue -> true
  | _ -> false

let%test "mixed constructor/less variant" =
  (match D.decode_string status_decoder {|{"Online": 10}|} with
  | Ok (Online 10) -> true
  | _ -> false)
  &&
  match D.decode_string status_decoder {|{"Offline": {}}|} with
  | Ok Offline -> true
  | _ -> false

let%test "my list" =
  (match D.decode_string my_list_decoder {|{"Null": "doesn't matter"}|} with
  | Ok Null -> true
  | _ -> false)
  &&
  match D.decode_string my_list_decoder {|{"L": {"Null": {}}}|} with
  | Ok (L Null) -> true
  | Ok _ -> false
  | Error e ->
      print_endline @@ D.string_of_error e;
      false

let%test "variant w/ record constructor" =
  (match D.decode_string constr_w_rec_decoder {|{"Empty": null}|} with
  | Ok Empty -> true
  | _ -> false)
  &&
  match D.decode_string constr_w_rec_decoder {|{"Item": {"i": -100}}|} with
  | Ok (Item { i = -100 }) -> true
  | _ -> false

let%test "non-mutually-recursive and binding types" =
  (match D.decode_string a_non_rec_decoder {|[-254, "hello"]|} with
  | Ok (-254, "hello") -> true
  | _ -> false)
  &&
  match D.decode_string b_non_rec_decoder {|false|} with
  | Ok false -> true
  | _ -> false

let%test "mutually-recursive decoders" =
  (match D.decode_string a_rec_decoder {|{"b" : null}|} with
  | Ok { b = None } -> true
  | _ -> false)
  && (match D.decode_string a_rec_decoder {|{"b" : {"a": null}}|} with
     | Ok { b = Some { a = None } } -> true
     | _ -> false)
  &&
  match D.decode_string b_rec_decoder {|{"a" : {"b": null}}|} with
  | Ok { a = Some { b = None } } -> true
  | _ -> false

let%test "complex mutually-recursive decoders" =
  (match D.decode_string a1_decoder {|{"l" : null, "m": null}|} with
  | Ok { l = None; m = None } -> true
  | _ -> false)
  &&
  match
    D.decode_string a1_decoder
      {|{"l" : {"n": {"o": {"l": null, "m": null}}}, "m": null}|}
  with
  | Ok { l = Some { n = { o = { l = None; m = None } } }; m = None } -> true
  | _ -> false

let%test "expression mutually-recursive decoder" =
  match
    D.decode_string expr_decoder
      {|{"BinOp" : [
       {"Add": {}},
       {"BinOp" : [{"Div": {}}, {"Num": 10}, {"Num": 5}]},
       {"BinOp" : [{"Mul": {}}, {"Num": 10}, {"Num": 3}]}
       ]}|}
  with
  | Ok (BinOp (Add, BinOp (Div, Num 10, Num 5), BinOp (Mul, Num 10, Num 3))) ->
      true
  | Ok _ -> false
  | Error e ->
      print_endline @@ D.string_of_error e;
      false

let%test "simple type var" =
  match D.decode_string int_record_wrapper_decoder {|{"wrapped":-2389}|} with
  | Ok { wrapped = -2389 } -> true
  | _ -> false

module Blah = struct
  type t = int [@@deriving decoders]
end

type blah_wrapped = Blah.t record_wrapper [@@deriving decoders]

let%test "basic module-wrapped type" =
  match D.decode_string blah_wrapped_decoder {|{"wrapped":10110}|} with
  | Ok { wrapped = 10110 } -> true
  | _ -> false

module Outer = struct
  module Inner = struct
    type t = string [@@deriving decoders]
  end
end

type outer_inner_wrapped = Outer.Inner.t record_wrapper [@@deriving decoders]

let%test "basic module-wrapped type" =
  match D.decode_string outer_inner_wrapped_decoder {|{"wrapped":"value"}|} with
  | Ok { wrapped = "value" } -> true
  | _ -> false

type ('a, 'b) double_wrap = { fst : 'a; snd : 'b } [@@deriving decoders]
type double_wrapped = (string, int) double_wrap [@@deriving decoders]

let%test "double type var" =
  match D.decode_string double_wrapped_decoder {|{"fst":"99","snd":100}|} with
  | Ok { fst = "99"; snd = 100 } -> true
  | _ -> false

module Ints = struct
  type my_int32 = int32 [@@deriving decoders]

  let%test "int32" =
    match D.decode_string my_int32_decoder "123445" with
    | Ok 123445l -> true
    | _ -> false

  type my_int32t = Int32.t [@@deriving decoders]

  let%test "int32t" =
    match D.decode_string my_int32t_decoder "5438" with
    | Ok 5438l -> true
    | _ -> false

  type my_int64 = int64 [@@deriving decoders]

  let%test "int64" =
    match D.decode_string my_int64_decoder "123445" with
    | Ok 123445L -> true
    | _ -> false

  type my_int64t = Int64.t [@@deriving decoders]

  let%test "int64t" =
    match D.decode_string my_int64t_decoder "5438" with
    | Ok 5438L -> true
    | _ -> false

  type my_nativeint = Nativeint.t [@@deriving decoders]

  let%test "my_nativeint" =
    match D.decode_string my_nativeint_decoder "5438" with
    | Ok 5438n -> true
    | _ -> false
end

type my_char = char [@@deriving decoders]

let%test "char" =
  (match D.decode_string my_char_decoder {|"c"|} with
  | Ok 'c' -> true
  | _ -> false)
  &&
  match D.decode_string my_char_decoder {|"abc"|} with
  (* We expect an error here because the string must have length 1 *)
  | Error _ -> true
  | _ -> false

type var = A | B
type open_var = var = A | B [@@deriving decoders]

let%test "type alias opened cstrs" =
  match D.decode_string open_var_decoder {|{"B":null}|} with
  | Ok B -> true
  | _ -> false
