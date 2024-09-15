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
(* type col = Blue | Red [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)
(* let _ = fun (_ : my_list) -> () *)

(* let my_list_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   let rec my_list_decoder_aux () = *)
(*     one_of *)
(*       [ *)
(*         ( "Null", *)
(*           string >>= function *)
(*           | "Null" -> succeed Null *)
(*           | e -> fail ("Could not decode " ^ e ^ " into " ^ "Null") ); *)
(*         ( "L", *)
(*           field "L" *)
(*             (\* (let ( >>=:: ) fst rest = uncons rest fst in *\) *)
(*             ( my_list_decoder_aux () >>= fun arg0 -> succeed (L arg0) ) ); *)
(*       ] *)
(*   in *)
(*   my_list_decoder_aux () *)

(* NOTE: Unfortunately, the BELOW is the correct implementation, rather than the above, which is the current implemtnation *)
let my_list_decoder =
  let open D in
  let open D.Infix in
  fix (fun my_list_decoder_aux ->
      one_of
        [
          ( "Null",
            string >>= function
            | "Null" -> succeed Null
            | e -> fail ("Could not decode " ^ e ^ " into " ^ "Null") );
          ( "L",
            field "L"
              (* (let ( >>=:: ) fst rest = uncons rest fst in *)
              ( my_list_decoder_aux >>= fun arg0 -> succeed (L arg0) ) );
        ])

(* let _ = *)
(*   match D.decode_string my_list_decoder "Null" with *)
(*   | Ok Null -> print_string "Found Null" *)
(*   | Ok (L Null) -> print_string "L Null" *)
(*   | Ok _ -> print_string "Went well" *)
(*   | _ -> print_string "Bad" *)

(* let my_list_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   let rec my_list_decoder_aux () = *)
(*     string >>= function *)
(*     | "Null" -> succeed Null *)
(*     | "L" -> my_list_decoder_aux () >>= fun arg0 -> succeed (L arg0) *)
(*     | _ -> fail "Failed" *)
(*   in *)

(*   my_list_decoder_aux () *)

(* let _ = my_list_decoder *)
(* open D *)

(* let rec my_list_decoder : my_list D.decoder = *)
(*   fix @@ fun my_list_decoder -> *)
(*   string >>= function *)
(*   | "Null" -> succeed Null *)
(*   | "L" -> *)
(*       (\* We expect the nested my_list for the L constructor *\) *)
(*       field "next" my_list_decoder >>= fun nested_list -> *)
(*       succeed (L nested_list) *)
(*   | _ -> fail "Expected 'Null' or 'L'" *)

(* let my_list_decoder : my_list D.decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   let rec my_list_decoder_aux () : my_list D.decoder = *)
(*     string >>= function *)
(*     | "Null" -> succeed Null *)
(*     | "L" -> *)
(*         (\* We expect the nested my_list for the L constructor *\) *)
(*         my_list_decoder_aux () >>= fun nested_list -> succeed (L nested_list) *)
(*     | _ -> fail "Expected 'Null' or 'L'" *)
(*   in *)
(*   my_list_decoder_aux () *)

(* type int *)

(* type a = { b : b option } *)
(* and b = { a : a option } [@@deriving decoders] *)

(* type c = { i : int; c : c option } *)

(* (\* Decoder for type 'a' *\) *)

(* let rec decode_a_aux () : a D.decoder = *)
(*   field "b" (maybe (decode_b_aux ())) |> map (fun b -> { b }) *)

(* and decode_b_aux () : b D.decoder = *)
(*   field "a" (maybe (decode_a_aux ())) |> map (fun a -> { a }) *)

(* let decode_a : a D.decoder = decode_a_aux () *)
(* let decode_b : b D.decoder = decode_b_aux () *)

(* type my_basic_cstr = Int of int [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)

(* let my_basic_cstr_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   D.string >>= function *)
(*   | "Int" -> *)
(*       let open D in *)
(*       let ( >>=:: ) fst rest = uncons rest fst in *)
(*       D.int >>=:: fun arg0 -> succeed (Int arg0) *)
(*   | s -> fail ("Could not decode" ^ s) *)

(* let _ = my_basic_cstr_decoder *)
