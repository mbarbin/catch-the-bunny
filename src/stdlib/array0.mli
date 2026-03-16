(*_********************************************************************************)
(*_  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*_  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ArrayLabels
end

val create : len:int -> 'a -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
