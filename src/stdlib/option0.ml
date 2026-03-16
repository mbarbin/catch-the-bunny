(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

include Stdlib.Option

let some_if cond a = if cond then Some a else None
