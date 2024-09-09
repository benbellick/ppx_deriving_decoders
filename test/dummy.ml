module D = Decoders_yojson.Safe.Decode

type my_basic_cstr2 = Ints of int * int | Strs of string * string
[@@deriving decoders]
