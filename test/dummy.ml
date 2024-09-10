module D = Decoders_yojson.Safe.Decode

(* type my_list = Null | Some of my_list [@@deriving_inline decoders] *)

(* let rec my_list_decoder = *)
(*   D.one_of *)
(*     [ *)
(*       ( "Null", *)
(*         let open D in *)
(*         let open D.Infix in *)
(*         D.string >>= function *)
(*         | "Null" -> succeed Null *)
(*         | e -> fail ("Could not decode " ^ e ^ " into " ^ "Null") ); *)
(*       ( "Some", *)
(*         D.field "Some" *)
(*           (let open D in *)
(*            let ( >>=:: ) fst rest = uncons rest fst in *)
(*            my_list_decoder >>=:: fun arg0 -> succeed (Some arg0)) ); *)
(*     ] *)

(* let _ = my_list_decoder *)

type my_list = Null | L of my_list

(* let rec my_list_decoder : my_list D.decoder = *)
(*   fix @@ fun my_list_decoder -> *)
(*   string >>= function *)
(*   | "Null" -> succeed Null *)
(*   | "L" -> *)
(*       (\* We expect the nested my_list for the L constructor *\) *)
(*       field "next" my_list_decoder >>= fun nested_list -> *)
(*       succeed (L nested_list) *)
(*   | _ -> fail "Expected 'Null' or 'L'" *)
