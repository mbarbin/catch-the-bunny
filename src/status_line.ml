open! Core

type t =
  { code : int
  ; may_be_present : bool array
  }
[@@deriving sexp_of]

let hash t = t.code
let hash_fold_t state t = hash_fold_int state t.code
let code t = t.code

let compute_code ~may_be_present =
  Array.fold may_be_present ~init:0 ~f:(fun acc j -> (2 * acc) + if j then 1 else 0)
;;

let create ~length ~code =
  let may_be_present = Array.create ~len:length false in
  let rec aux i code =
    if code > 0 && i < length
    then (
      may_be_present.(i) <- code mod 2 = 1;
      aux (i + 1) (code / 2))
  in
  aux 0 code;
  { code; may_be_present }
;;

let copy t = { code = t.code; may_be_present = Array.copy t.may_be_present }
let length t = Array.length t.may_be_present

let may_be_present t ~index =
  if index < 0 || index >= length t then false else t.may_be_present.(index)
;;

let remove t ~index =
  let may_be_present = Array.copy t.may_be_present in
  if index >= 0 && index < length t then may_be_present.(index) <- false;
  { code = compute_code ~may_be_present; may_be_present }
;;

let move t =
  let length = length t in
  let may_be_present = Array.create ~len:length false in
  for i = 0 to pred length do
    if t.may_be_present.(i)
    then
      for j = -1 to 1 do
        if j <> 0
        then (
          let index = i + j in
          if index >= 0 && index < length then may_be_present.(index) <- true)
      done
  done;
  { code = compute_code ~may_be_present; may_be_present }
;;
