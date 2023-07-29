open! Core

(** To solve the game, we create a big automaton that encodes the transitions
    between all possible status lines. Once that automaton is computed, the
    search for a solution boils down to finding a particular path within it :
    a path that starts from the initial state and that leads to a state where
    the location of the bunny is known.

    The vertices of the automaton are all the different status lines that may be
    created. The edges are directed, and labelled with and index of the box. An
    edge [b] from state [s1 -> s2] means that given the information [s1], if we
    choose to open the box [b] and the bunny is not there, and then it moves,
    then we're left in a state with known information [s2].

    Not all status lines are reachable from the initial state, meaning some
    vertices have no incoming edges. *)

type t

(** Compute the complete automaton with its vertices and edges for a given size. *)
val create : size:int -> t

(** Returns the edges that are originating from the vertex with the given code.
    For each edge we return its label along with its targeting vertex. *)
val edges : t -> code:int -> (int * Status_line.t) list

(** Return the edges that are targeting the vertex with the given code. *)
val reverse_edges : t -> code:int -> (int * Status_line.t) list

module Step : sig
  (** Human readable steps that are happening as the game goes on. *)
  type t =
    | Open_box of int
    | Status_line of Status_line.t
    | Bunny_moved of Status_line.t
    | Bunny_was_caught
  [@@deriving sexp_of]
end

(** Test a candidate solution, and returns its human readable execution trace.
    The bunny is caught at the end of the given sequence if it is indeed a
    valid solution. *)
val execute_sequence : t -> sequence:int list -> Step.t list

module Solution : sig
  type t =
    { length : int
    ; sequence : int list
    }
  [@@deriving sexp_of]
end

(** Find a few sequences that catch the bunny, ordered by increasing number of
    steps. This function does not return all possible solutions, rather only a
    few among those which minimizes the number of steps required. *)
val catch_the_bunny : t -> Solution.t list
