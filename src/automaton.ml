open! Core

(** Encode the transitions between status lines. *)

type t = unit

let create ~length =
  let code = Int.of_float (2. ** Int.to_float length) in
  let status_line = Status_line.create ~length ~code in
  ignore status_line
;;
