module D = Decoders_yojson.Safe.Decode

type my_int = int [@@deriving decoders]

(* type my_int32 = int32 [@@deriving decoders] *)
(* type my_int64 = int64 [@@deriving decoders] *)
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
  match D.decode_string my_basic_cstr_decoder {|{"Int": [10]}|} with
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
      {|{"basic" : {"i": 10}, "cstr": {"Int": [10]}}|}
  with
  | Ok b -> b = { basic = { i = 10 }; cstr = Int 10 }
  | Error _ -> false
