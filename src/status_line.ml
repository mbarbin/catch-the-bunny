open! Core

type t =
  { code : int
  ; may_be_located : bool array
  }
[@@deriving compare, equal, sexp_of]

let code t = t.code

let compute_code ~may_be_located =
  Array.fold may_be_located ~init:0 ~f:(fun acc j -> (2 * acc) + if j then 1 else 0)
;;

let create ~size ~code =
  if code < 0 then raise_s [%sexp "invalid negative code", { size : int; code : int }];
  let may_be_located = Array.create ~len:size false in
  let rec aux i code =
    if i >= 0 && i < size
    then (
      may_be_located.(i) <- code mod 2 = 1;
      aux (i - 1) (code / 2))
    else code
  in
  let remainder = aux (size - 1) code in
  if remainder <> 0 then raise_s [%sexp "code out of bounds", { size : int; code : int }];
  let code' = compute_code ~may_be_located in
  if code <> code'
  then raise_s [%sexp "code does not round trip", { code : int; code' : int }];
  { code; may_be_located }
;;

let size t = Array.length t.may_be_located

let remove t ~index =
  let size = size t in
  let may_be_located = Array.copy t.may_be_located in
  if index >= 0 && index < size
  then may_be_located.(index) <- false
  else raise_s [%sexp "index out of bounds", { size : int; index : int }];
  { code = compute_code ~may_be_located; may_be_located }
;;

let move t =
  let size = size t in
  let may_be_located = Array.create ~len:size false in
  for i = 0 to pred size do
    if t.may_be_located.(i)
    then
      Array.iter [| -1; 1 |] ~f:(fun j ->
        let index = i + j in
        if index >= 0 && index < size then may_be_located.(index) <- true)
  done;
  { code = compute_code ~may_be_located; may_be_located }
;;

let catch_the_bunny t =
  if 1 = Array.count t.may_be_located ~f:Fn.id
  then Array.find_mapi t.may_be_located ~f:(fun i b -> Option.some_if b i)
  else None
;;
