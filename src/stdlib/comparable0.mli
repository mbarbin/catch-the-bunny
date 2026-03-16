(*_********************************************************************************)
(*_  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*_  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

module type S = sig
  type t

  val compare : t -> t -> Ordering.t
end
