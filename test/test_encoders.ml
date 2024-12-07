module E = Decoders_yojson.Safe.Encode

type int_wrap = int [@@deriving encoders]
type int_list = int list [@@deriving encoders]
type int_array = int array [@@deriving encoders]
type wrapped_int = { int : int } [@@deriving encoders]
type wrapped_int_string = { i : int; s : string } [@@deriving encoders]
type int_string = int * string [@@deriving encoders]

type vars =
  | Int of int
  | Str of string
  | Tup of int * string
  (* | Rec of { i : int; s : string } *)
  | Nothing
[@@deriving encoders]

let vars_encoder = function
  | Int arg0 -> E.obj [ ("Int", E.int arg0) ]
  | Str arg0 -> E.obj [ ("Str", E.string arg0) ]
  | Tup (arg0, arg1) ->
      E.obj [ ("Tup", E.list E.value [ E.int arg0; E.string arg1 ]) ]
  | Nothing -> E.obj [ ("Nothing", E.null) ]

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
  &&
  match E.encode_string vars_encoder Nothing with
  | {|{"Nothing":null}|} -> true
  | _ -> false
