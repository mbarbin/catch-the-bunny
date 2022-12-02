open! Core

(** Encode the transitions between status lines. *)

type t

val create : length:int -> t
val find_status_line : t -> code:int -> Status_line.t option
val edges : t -> code:int -> (int * Status_line.t) list
val reverse_edges : t -> code:int -> (int * Status_line.t) list

module Step : sig
  type t =
    | Open_box of int
    | Status_line of Status_line.t
    | Bunny_moved of Status_line.t
    | Bunny_was_caught
  [@@deriving sexp_of]
end

val sequence : t -> int list -> Step.t list

(** Find a sequence that catches the bunny in a minimum number of steps. *)
val catch_bunny : t -> (int * int list) list list
