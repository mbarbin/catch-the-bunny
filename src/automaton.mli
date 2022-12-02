open! Core

(** Encode the transitions between status lines. *)

type t

val create : length:int -> t
val find_status_line : t -> code:int -> Status_line.t option
val edges : t -> code:int -> (int * Status_line.t) list
val reverse_edges : t -> code:int -> (int * Status_line.t) list
