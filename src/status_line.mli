open! Core

(** A status line indicates the columns where the bunny may be located
   on that line. As the game goes on and more information is gathered,
   we're hoping that the set of possible locations will be decreased
   down to a single column, so that we can catch the bunny at that
   location. *)

type t [@@deriving equal, hash, sexp_of]

(** Status lines are encoded with an int between 0 and (2**size - 1). *)
val code : t -> int

(** Create a new status line with the given size and code. Raises when
   called with a code that exceeds the code range. *)
val create : size:int -> code:int -> t

(** Update the information known upon witnessing that the bunny is not
   located at a given box that was just opened. *)
val remove : t -> index:int -> t

(** Given the information we currently know, return the new
   information we will know after the bunny moves from its current
   location. *)
val move : t -> t

(** If there is only one single column where the bunny may currently
   be located, return that column, and [None] otherwise. *)
val catch_the_bunny : t -> int option
