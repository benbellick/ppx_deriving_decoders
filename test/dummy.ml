module D = Decoders_yojson.Safe.Decode

module Blah (E : Decoders.Encode.S) = struct
  type int_wrap = int
  and int_list = int list
  and str = string
  and int_str = int * string [@@deriving encoders]

  [@@@deriving.end]
end
(* type my_list = Null | L of my_list [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)

(* let _ = fun (_ : my_list) -> () *)

(* let my_list_decoder = *)
(*   D.fix (fun my_list_decoder_aux -> *)
(*       let open D in *)
(*       one_of *)
(*         [ *)
(*           ( "Null", *)
(*             D.string >>= function "Null" -> succeed Null | _ -> fail "Failure" *)
(*           ); *)
(*           ( "L", *)
(*             D.field "L" *)
(*               (let open D in *)
(*                let ( >>=:: ) fst rest = uncons rest fst in *)
(*                my_list_aux >>=:: fun arg0 -> succeed (L arg0)) ); *)
(*         ]) *)

(* let _ = my_list_decoder *)

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
(* let my_list_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   fix (fun my_list_decoder_aux -> *)
(*       one_of *)
(*         [ *)
(*           ( "Null", *)
(*             string >>= function *)
(*             | "Null" -> succeed Null *)
(*             | e -> fail ("Could not decode " ^ e ^ " into " ^ "Null") ); *)
(*           ( "L", *)
(*             field "L" *)
(*               (\* (let ( >>=:: ) fst rest = uncons rest fst in *\) *)
(*               ( my_list_decoder_aux >>= fun arg0 -> succeed (L arg0) ) ); *)
(*         ]) *)

(* let _ = *)
(*   match D.decode_string my_list_decoder {|{"L": "Null"}|} with *)
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
(* and b = { a : a option } [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)

(* let a_decoder b_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   let* b = field "b" (D.nullable b_decoder) in *)
(*   succeed { b } *)

(* let _ = a_decoder *)

(* let b_decoder b_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   let* a = field "a" (D.nullable (a_decoder b_decoder)) in *)
(*   succeed { a } *)

(* let b_decoder = D.fix b_decoder *)
(* let a_decoder = a_decoder b_decoder *)

(* let a_decoder b_decoder : a D.decoder = *)
(*   let open D in *)
(*   let* b = field "b" (D.nullable b_decoder) in *)
(*   succeed { b } *)

(* let b_decoder b_decoder : b D.decoder = *)
(*   let open D in *)
(*   let* a = field "a" (D.nullable (a_decoder b_decoder)) in *)
(*   succeed { a } *)

(* let b_decoder = D.fix b_decoder *)
(* let a_decoder = a_decoder b_decoder *)

(* let _ = *)
(*   match D.decode_string a_decoder {|{"b": {"a": null}}|} with *)
(*   | Ok { b = Some { a = None } } -> print_string "yuh" *)
(*   | _ -> print_string "nuh" *)

(* let _ = *)
(*   match D.decode_string b_decoder {|{"a": {"b": null}}|} with *)
(*   | Ok { a = Some { b = None } } -> print_string "yuh2" *)
(*   | _ -> print_string "nuh2" *)

type a1 = { l : b1 option; m : c1 option }
and b1 = { n : c1 }
and c1 = { o : a1 }

let a1_decoder b1_decoder c1_decoder =
  let open D in
  let open D.Infix in
  let* l = field "l" (D.nullable b1_decoder) in
  let* m = field "m" (D.nullable c1_decoder) in
  succeed { l; m }

let b1_decoder c1_decoder =
  let open D in
  let open D.Infix in
  let* n = field "n" c1_decoder in
  succeed { n }

let c1_decoder c1_decoder =
  let open D in
  let open D.Infix in
  let* o = field "o" (a1_decoder (b1_decoder c1_decoder) c1_decoder) in
  succeed { o }

let c1_decoder = D.fix c1_decoder
let b1_decoder = b1_decoder c1_decoder
let a1_decoder = a1_decoder b1_decoder c1_decoder

(* let a1_decoder b1_decoder c1_decoder : a1 D.decoder = *)
(*   let open D in *)
(*   let* l = field "l" (D.nullable b1_decoder) in *)
(*   let* m = field "m" (D.nullable c1_decoder) in *)
(*   succeed { l; m } *)

(* let b1_decoder c1_decoder : b1 D.decoder = *)
(*   let open D in *)
(*   let* n = field "n" c1_decoder in *)
(*   succeed { n } *)

(* let c1_decoder c1_decoder : c1 D.decoder = *)
(*   let open D in *)
(*   let b1_decoder = b1_decoder c1_decoder in *)
(*   let a1_decoder = a1_decoder b1_decoder c1_decoder in *)
(*   let* o = field "o" a1_decoder in *)
(*   succeed { o } *)

(* let c1_decoder = D.fix c1_decoder *)
(* let b1_decoder = b1_decoder c1_decoder *)
(* let a1_decoder = a1_decoder b1_decoder c1_decoder *)

(* let b_decoder b_decoder : b D.decoder = *)
(*   let open D in *)
(*   let* a = field "a" (D.nullable (a_decoder b_decoder)) in *)
(*   succeed { a } *)

(* let b_decoder = D.fix b_decoder *)
(* let a_decoder = a_decoder b_decoder *)

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
(* type my_basic_cstr = Int of int [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)

(* let my_basic_cstr_decoder = *)
(*   let open D in *)
(*   let open D.Infix in *)
(*   one_of *)
(*     [ *)
(*       ( "Int", *)
(*         D.field "Int" *)
(*           (let ( >>=:: ) fst rest = uncons rest fst in *)
(*            D.int >>=:: fun arg0 -> succeed (Int arg0)) ); *)
(*     ] *)

(* let _ = my_basic_cstr_decoder *)
(* (\* let _ = *\) *)
(* (\*   match D.decode_string my_basic_cstr_decoder {|{"Int": [10]}|} with *\) *)
(* (\*   | Ok (Int 10) -> print_string "We did it!" *\) *)
(* (\*   | Ok _ -> print_string "Weird" *\) *)
(* (\*   | Error e -> print_string (D.string_of_error e) *\) *)
(* type colors = Red | Blue | Green [@@deriving_inline decoders] *)

(* [@@@deriving.end] *)

(* (\* THIS is what should be coming out... So that we can handle some cases where we aren't just a constructor *\) *)
(* let colors_decoder = *)
(*   let open D in *)
(*   one_of *)
(*     [ *)
(*       ( "Red", *)
(*         string >>= function "Red" -> succeed Red | _ -> fail "Expected 'Red'" ); *)
(*       ( "Blue", *)
(*         string >>= function *)
(*         | "Blue" -> succeed Blue *)
(*         | _ -> fail "Expected 'Blue'" ); *)
(*       ( "Green", *)
(*         string >>= function *)
(*         | "Green" -> succeed Green *)
(*         | _ -> fail "Expected 'Green'" ); *)
(*     ] *)

(* let () = *)
(*   match D.decode_string colors_decoder {|"Blue"|} with *)
(*   | Ok Blue -> print_string "We did it" *)
(*   | _ -> print_string "We did not :( " *)
