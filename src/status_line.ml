module May_be_located = struct
  type t = bool array

  let sexp_of_t t =
    Sexp.Atom (String.init (Array.length t) ~f:(fun i -> if t.(i) then '1' else '0'))
  ;;
end

type t =
  { code : int
  ; may_be_located : May_be_located.t
  }
[@@deriving sexp_of]

let equal t1 t2 = t1.code = t2.code
let compare t1 t2 = Int.compare t1.code t2.code
let code t = t.code

let compute_code ~may_be_located =
  Array.fold may_be_located ~init:0 ~f:(fun acc j -> (2 * acc) + if j then 1 else 0)
;;

let check_code_exn ~size ~code =
  if code < 0 || code >= 1 lsl size
  then raise_s [%sexp "code out of bounds", { size : int; code : int }]
;;

let check_size_exn ~size =
  if size < 1 then raise_s [%sexp "invalid size, expected >= 1", { size : int }]
;;

let create ~size ~code =
  check_size_exn ~size;
  check_code_exn ~size ~code;
  let may_be_located = Array.create ~len:size false in
  let rec aux i code =
    if i >= 0 && i < size
    then (
      may_be_located.(i) <- code % 2 = 1;
      aux (i - 1) (code / 2))
    else code
  in
  let remainder = aux (size - 1) code in
  assert (remainder = 0);
  let code' = compute_code ~may_be_located in
  if code <> code'
  then
    raise_s
      [%sexp "code does not round trip", { code : int; code' : int }] [@coverage off];
  { code; may_be_located }
;;

let size t = Array.length t.may_be_located

let check_index_exn t ~index =
  let size = size t in
  if index < 0 || index >= size
  then raise_s [%sexp "index out of bounds", { size : int; index : int }]
;;

let remove t ~index =
  check_index_exn t ~index;
  let may_be_located = Array.copy t.may_be_located in
  may_be_located.(index) <- false;
  { code = compute_code ~may_be_located; may_be_located }
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
  { code = compute_code ~may_be_located; may_be_located }
;;

let catch_the_bunny t =
  if 1 = Array.count t.may_be_located ~f:Fn.id
  then Array.find_mapi t.may_be_located ~f:(fun i b -> Option.some_if b i)
  else None
;;
