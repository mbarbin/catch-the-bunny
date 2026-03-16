(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

include Stdlib.ListLabels

let compare (type a) (module M : Comparable0.S with type t = a) t1 t2 =
  let cmp a b = Ordering.to_int (M.compare a b) in
  compare ~cmp t1 t2 |> Ordering.of_int
;;

let find_exn t ~f = find ~f t
let init len ~f = init ~len ~f
let iter t ~f = iter ~f t

let sort (type a) (module M : Comparable0.S with type t = a) t =
  let cmp a b = Ordering.to_int (M.compare a b) in
  sort t ~cmp
;;
