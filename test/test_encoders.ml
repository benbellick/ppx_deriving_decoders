module E = Decoders_yojson.Safe.Encode

type int_wrap = int [@@deriving encoders]
type int_list = int list [@@deriving encoders]
type int_array = int array [@@deriving encoders]
type wrapped_int = { int : int } [@@deriving encoders]
type wrapped_int_string = { i : int; s : string } [@@deriving encoders]
type int_string = int * string [@@deriving encoders]
type basic_recur = Empty | Rec of basic_recur [@@deriving encoders]

type expr = Num of int | BinOp of op * expr * expr
and op = Add | Sub | Mul | Div [@@deriving encoders]

type vars =
  | Int of int
  | Str of string
  | Tup of int * string
  | Rec of { i : int; s : string }
  | Nothing
[@@deriving encoders]

let%test "int_wrap" =
  match E.encode_string int_wrap_encoder 1234 with "1234" -> true | _ -> false

let%test "int_list" =
  match E.encode_string int_list_encoder [ 1; 2; 3; 4 ] with
  | {|[1,2,3,4]|} -> true
  | _ -> false

let%test "int_array" =
  match E.encode_string int_array_encoder [| 1; 2; 3; 4 |] with
  | {|[1,2,3,4]|} -> true
  | _ -> false

let%test "wrapped_int" =
  match E.encode_string wrapped_int_encoder { int = 101 } with
  | {|{"int":101}|} -> true
  | _ -> false

let%test "wrapped_int_string_string" =
  match E.encode_string wrapped_int_string_encoder { i = -10; s = "super" } with
  | {|{"i":-10,"s":"super"}|} -> true
  | _ -> false

let%test "int_string" =
  match E.encode_string int_string_encoder (15, "the string") with
  | {|[15,"the string"]|} -> true
  | _ -> false

let%test "vars" =
  (match E.encode_string vars_encoder (Int 10) with
  | {|{"Int":10}|} -> true
  | _ -> false)
  && (match E.encode_string vars_encoder (Str "something") with
     | {|{"Str":"something"}|} -> true
     | _ -> false)
  && (match E.encode_string vars_encoder (Tup (43, "another")) with
     | {|{"Tup":[43,"another"]}|} -> true
     | _ -> false)
  && (match E.encode_string vars_encoder (Rec { i = -43; s = "inner" }) with
     | {|{"Rec":{"i":-43,"s":"inner"}}|} -> true
     | _ -> false)
  &&
  match E.encode_string vars_encoder Nothing with
  | {|{"Nothing":null}|} -> true
  | _ -> false

let%test "basic_recursion" =
  match E.encode_string basic_recur_encoder (Rec (Rec Empty)) with
  | {|{"Rec":{"Rec":{"Empty":null}}}|} -> true
  | _ -> false

type 'a record_wrapper = { wrapped : 'a } [@@deriving encoders]
type int_record_wrapper = int record_wrapper [@@deriving encoders]

let%test "basic type var" =
  match E.encode_string int_record_wrapper_encoder { wrapped = 9876 } with
  | {|{"wrapped":9876}|} -> true
  | _ -> false

type ('a, 'b) double_wrap = { fst : 'a; snd : 'b } [@@deriving encoders]
type double_wrapped = (string, int) double_wrap [@@deriving encoders]

let%test "double type var" =
  match E.encode_string double_wrapped_encoder { fst = "9"; snd = 10 } with
  | {|{"fst":"9","snd":10}|} -> true
  | _ -> false

module Outer = struct
  module Inner = struct
    type t = string [@@deriving encoders]
  end
end

type outer_inner_wrapped = Outer.Inner.t record_wrapper [@@deriving encoders]

let%test "module wrapped" =
  match E.encode_string outer_inner_wrapped_encoder { wrapped = "a thing" } with
  | {|{"wrapped":"a thing"}|} -> true
  | _ -> false

module Ints = struct
  type my_int32 = int32 [@@deriving encoders]

  let%test "int32" =
    match E.encode_string my_int32_encoder 123445l with
    | "123445" -> true
    | _ -> false

  type my_int32t = Int32.t [@@deriving encoders]

  let%test "int32t" =
    match E.encode_string my_int32t_encoder 5438l with
    | "5438" -> true
    | _ -> false

  type my_int64 = int64 [@@deriving encoders]

  let%test "int64" =
    match E.encode_string my_int64_encoder 123445L with
    | "123445" -> true
    | _ -> false

  type my_int64t = Int64.t [@@deriving encoders]

  let%test "int64t" =
    match E.encode_string my_int64t_encoder 5438L with
    | "5438" -> true
    | _ -> false

  type my_nativeint = Nativeint.t [@@deriving encoders]

  let%test "my_nativeint" =
    match E.encode_string my_nativeint_encoder 5438n with
    | "5438" -> true
    | _ -> false
end
