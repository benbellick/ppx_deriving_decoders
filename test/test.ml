module D = Decoders_yojson.Safe.Decode

type t = int [@@deriving decoders]
type t1 = int [@@deriving_inline decoders]

[@@@deriving.end]

type t2 = int32 [@@deriving_inline decoders]

[@@@deriving.end]
(* let r = D.decode_string t1_decoder "1" *)

(* let () = *)
(* match r with *)
(* | Ok s -> Format.printf "%i" s *)
(* | Error _ -> Format.printf "Parse Error" *)
