(*_********************************************************************************)
(*_  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*_  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.Int
end

val compare : t -> t -> Ordering.t
val to_dyn : t -> Dyn.t
