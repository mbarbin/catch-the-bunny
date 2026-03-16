(*_********************************************************************************)
(*_  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*_  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ListLabels
end

val compare : (module Comparable0.S with type t = 'a) -> 'a t -> 'a t -> Ordering.t
val find_exn : 'a t -> f:('a -> bool) -> 'a
val init : int -> f:(int -> 'a) -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val sort : (module Comparable0.S with type t = 'a) -> 'a t -> 'a t
