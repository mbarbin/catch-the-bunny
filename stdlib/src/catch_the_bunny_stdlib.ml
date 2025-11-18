(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module Dyn = Dyn0
module Ordering = Ordering

module Comparable = struct
  module type S = sig
    type t

    val compare : t -> t -> Ordering.t
  end
end

module Array = struct
  include Stdlib.ArrayLabels

  let create ~len a = make len a
  let iter t ~f = iter ~f t
  let fold t ~init ~f = fold_left t ~init ~f
end

module Int = struct
  include Stdlib.Int

  let compare a b = Ordering.of_int (Int.compare a b)
  let to_dyn = Dyn.int
end

module List = struct
  include Stdlib.ListLabels

  let compare (type a) (module M : Comparable.S with type t = a) t1 t2 =
    let cmp a b = Ordering.to_int (M.compare a b) in
    compare ~cmp t1 t2 |> Ordering.of_int
  ;;

  let find_exn t ~f = find ~f t
  let init len ~f = init ~len ~f
  let iter t ~f = iter ~f t

  let sort (type a) (module M : Comparable.S with type t = a) t =
    let cmp a b = Ordering.to_int (M.compare a b) in
    sort t ~cmp
  ;;
end

module Option = struct
  include Stdlib.Option

  let some_if cond a = if cond then Some a else None
end

module String = struct
  include Stdlib.StringLabels
end

let require_does_raise f =
  match f () with
  | _ -> Dyn.raise "Did not raise." []
  | exception e -> print_endline (Printexc.to_string e)
;;

module With_equal_and_dyn = struct
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

let require_equal
      (type a)
      (module M : With_equal_and_dyn.S with type t = a)
      (v1 : a)
      (v2 : a)
  =
  if not (M.equal v1 v2)
  then Dyn.raise "Values are not equal." [ "v1", v1 |> M.to_dyn; "v2", v2 |> M.to_dyn ]
;;
