module E = Decoders_yojson.Safe.Encode

type int_wrap = int [@@deriving encoders]
type int_list = int list [@@deriving encoders]
type int_array = int array [@@deriving encoders]
type wrapped_int = { int : int } [@@deriving encoders]
type wrapped_int_string = { i : int; s : string } [@@deriving encoders]

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
