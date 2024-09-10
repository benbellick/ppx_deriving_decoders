module D = Decoders_yojson.Safe.Decode

(* type color = Red of int | Blue of int [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)

(* type c2 = Red *)

(* let dec : c2 D.decoder = *)
(*   let open D.Infix in *)
(*   D.string >>= function "Red" -> D.succeed Red | _ -> D.succeed Red *)
(* type colors = Red | Blue | Green [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)
