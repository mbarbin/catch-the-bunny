(*_********************************************************************************)
(*_  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*_  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

module Array = Array0
module Code_error = Code_error0
module Comparable = Comparable0
module Dyn = Dyn0
module Int = Int0
module List = List0
module Option = Option0
module Ordering = Ordering0
module String = String0
module With_equal_and_dyn = With_equal_and_dyn0

val print_dyn : Dyn.t -> unit
val require_does_raise : (unit -> 'a) -> unit
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
