(* Since I don't have WIFI and don't have an encoder implementation installed, I'm temporarily just checking for compilation *)

module CompileTest (E : Decoders.Encode.S) = struct
  type int_wrap = int
  and int_list = int list

  and int_array = int array
  (* and wrapped_int = { int : int } *) [@@deriving encoders]

  [@@@deriving.end]
end

(* want it to look like *)
module Encode (E : Decoders.Encode.S) = struct
  int_array = E.obj [ ("int", E.int int) ]
end
