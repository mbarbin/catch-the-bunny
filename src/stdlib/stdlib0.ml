(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

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

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)

let require_does_raise f =
  match f () with
  | _ -> Code_error.raise "Did not raise." []
  | exception e -> print_endline (Printexc.to_string e)
;;

let require_equal
      (type a)
      (module M : With_equal_and_dyn.S with type t = a)
      (v1 : a)
      (v2 : a)
  =
  if not (M.equal v1 v2)
  then
    Code_error.raise
      "Values are not equal."
      [ "v1", v1 |> M.to_dyn; "v2", v2 |> M.to_dyn ]
;;
