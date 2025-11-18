(*_********************************************************************************)
(*_  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*_  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

(** Extending [Stdlib] for use in the project. *)

module Dyn = Dyn0
module Ordering = Ordering

module Comparable : sig
  module type S = sig
    type t

    val compare : t -> t -> Ordering.t
  end
end

module Array : sig
  include module type of struct
    include Stdlib.ArrayLabels
  end

  val create : len:int -> 'a -> 'a t
  val iter : 'a t -> f:('a -> unit) -> unit
  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end

module Int : sig
  include module type of struct
    include Stdlib.Int
  end

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
end

module List : sig
  include module type of struct
    include Stdlib.ListLabels
  end

  val compare : (module Comparable.S with type t = 'a) -> 'a t -> 'a t -> Ordering.t
  val find_exn : 'a t -> f:('a -> bool) -> 'a
  val init : int -> f:(int -> 'a) -> 'a t
  val iter : 'a t -> f:('a -> unit) -> unit
  val sort : (module Comparable.S with type t = 'a) -> 'a t -> 'a t
end

module Option : sig
  include module type of struct
    include Stdlib.Option
  end

  val some_if : bool -> 'a -> 'a t
end

module String : sig
  include module type of struct
    include Stdlib.StringLabels
  end
end

val require_does_raise : (unit -> 'a) -> unit

module With_equal_and_dyn : sig
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
