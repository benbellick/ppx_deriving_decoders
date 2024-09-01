module D = Decoders_yojson.Safe.Decode

type t = int * string * bool

let dec : t D.decoder =
  let open D in
  int
  |> uncons (fun i ->
         string |> uncons (fun s -> bool |> uncons (fun b -> succeed (i, s, b))))
