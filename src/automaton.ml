open! Core

(** Encode the transitions between status lines. *)

module Vertex = struct
  type t =
    { status_line : Status_line.t
    ; edges : (int * Status_line.t) Queue.t
    ; reverse_edges : (int * Status_line.t) Queue.t
    }
end

type t = { vertices : Vertex.t array }

let create ~length =
  let max_code = Int.of_float (2. ** Int.to_float length) - 1 in
  let vertices =
    Array.create
      ~len:(max_code + 1)
      { Vertex.status_line = Status_line.create ~length ~code:max_code
      ; edges = Queue.create ()
      ; reverse_edges = Queue.create ()
      }
  in
  for code = 0 to max_code do
    vertices.(code)
      <- { Vertex.status_line = Status_line.create ~length ~code
         ; edges = Queue.create ()
         ; reverse_edges = Queue.create ()
         }
  done;
  for i = 0 to max_code do
    let vertex = vertices.(i) in
    for j = 0 to pred length do
      let status_line =
        Status_line.remove vertex.status_line ~index:j |> Status_line.move
      in
      Queue.enqueue vertex.edges (j, status_line)
    done
  done;
  for i = 0 to max_code do
    Queue.iter vertices.(i).edges ~f:(fun (j, status_line) ->
      let code = Status_line.code status_line in
      Queue.enqueue vertices.(code).reverse_edges (j, vertices.(i).status_line))
  done;
  { vertices }
;;

let find_status_line t ~code =
  if code < 0 || code >= Array.length t.vertices
  then None
  else Some t.vertices.(code).status_line
;;

let edges t ~code =
  if code < 0 || code >= Array.length t.vertices
  then []
  else Queue.to_list t.vertices.(code).edges
;;

let reverse_edges t ~code =
  if code < 0 || code >= Array.length t.vertices
  then []
  else Queue.to_list t.vertices.(code).reverse_edges
;;

module Step = struct
  type t =
    | Open_box of int
    | Status_line of Status_line.t
    | Bunny_moved of Status_line.t
    | Bunny_was_caught
  [@@deriving sexp_of]
end

let sequence t indexes =
  let max_code = Array.length t.vertices - 1 in
  let all = t.vertices.(max_code).status_line in
  let rec aux acc code = function
    | [] -> List.rev acc
    | hd :: tl ->
      let updated_status_line =
        Status_line.remove t.vertices.(code).status_line ~index:hd
      in
      let _, bunny_moved =
        Queue.find t.vertices.(code).edges ~f:(fun (i, _) -> i = hd)
        |> Option.value_exn ~here:[%here]
      in
      aux
        Step.(
          Bunny_moved bunny_moved :: Status_line updated_status_line :: Open_box hd :: acc)
        (Status_line.code bunny_moved)
        tl
  in
  aux [ Status_line all ] max_code indexes
;;
