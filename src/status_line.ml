(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module May_be_located = struct
  type t = bool array

  let to_dyn t =
    Dyn.string (String.init (Array.length t) ~f:(fun i -> if t.(i) then '1' else '0'))
  ;;

  let compute_code (t : t) =
    Array.fold t ~init:0 ~f:(fun acc j -> (2 * acc) + if j then 1 else 0)
  ;;

  let is_singleton t =
    let exception Return in
    match
      Array.fold t ~init:0 ~f:(fun acc b ->
        if b then if acc > 0 then raise_notrace Return else 1 else acc)
    with
    | 1 -> true
    | _ -> false
    | exception Return -> false
  ;;
end

type t =
  { code : int
  ; may_be_located : May_be_located.t
  }

let to_dyn { code; may_be_located } =
  Dyn.record
    [ "code", Dyn.int code; "may_be_located", May_be_located.to_dyn may_be_located ]
;;

let equal t1 t2 = t1.code = t2.code
let compare t1 t2 = Int.compare t1.code t2.code
let code t = t.code

let check_code_exn ~size ~code =
  if code < 0 || code >= 1 lsl size
  then
    Dyn.raise "Code out of bounds." [ "size", size |> Dyn.int; "code", code |> Dyn.int ]
;;

let check_size_exn ~size =
  if size < 1 then Dyn.raise "Invalid size, expected [>= 1]." [ "size", size |> Dyn.int ]
;;

let check_code_does_round_trip_exn t ~expected =
  let computed = May_be_located.compute_code t.may_be_located in
  if expected <> computed
  then
    Dyn.raise
      "Code does not round trip."
      [ "expected", expected |> Dyn.int; "computed", computed |> Dyn.int ]
;;

let create ~size ~code =
  check_size_exn ~size;
  check_code_exn ~size ~code;
  let may_be_located = Array.create ~len:size false in
  let rec aux i code =
    if i >= 0 && i < size
    then (
      may_be_located.(i) <- code mod 2 = 1;
      aux (i - 1) (code / 2))
    else code
  in
  let remainder = aux (size - 1) code in
  assert (remainder = 0);
  let t = { code; may_be_located } in
  check_code_does_round_trip_exn t ~expected:code;
  t
;;

let size t = Array.length t.may_be_located

let check_index_exn t ~index =
  let size = size t in
  if index < 0 || index >= size
  then
    Dyn.raise
      "Index out of bounds."
      [ "size", size |> Dyn.int; "index", index |> Dyn.int ]
;;

let remove t ~index =
  check_index_exn t ~index;
  let may_be_located = Array.copy t.may_be_located in
  may_be_located.(index) <- false;
  { code = May_be_located.compute_code may_be_located; may_be_located }
;;

let move t =
  let size = size t in
  let may_be_located = Array.create ~len:size false in
  for i = 0 to Int.pred size do
    if t.may_be_located.(i)
    then
      Array.iter [| -1; 1 |] ~f:(fun j ->
        let index = i + j in
        if index >= 0 && index < size then may_be_located.(index) <- true)
  done;
  { code = May_be_located.compute_code may_be_located; may_be_located }
;;

let may_catch_the_bunny t =
  if May_be_located.is_singleton t.may_be_located
  then Array.find_mapi t.may_be_located ~f:(fun i b -> Option.some_if b i)
  else None
;;

let bunny_was_caught t ~index =
  check_index_exn t ~index;
  t.may_be_located.(index) && May_be_located.is_singleton t.may_be_located
;;
