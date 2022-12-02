open! Core

(** A status line indicates the columns where the bunny may be on that line. *)
type t [@@deriving equal, hash, sexp_of]

val code : t -> int
val create : length:int -> code:int -> t
val copy : t -> t
val length : t -> int
val may_be_present : t -> index:int -> bool
val remove : t -> index:int -> t
val move : t -> t
val catch_bunny : t -> int option
