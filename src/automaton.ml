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
      let code =
        vertex.status_line
        |> Status_line.remove ~index:j
        |> Status_line.move
        |> Status_line.code
      in
      Queue.enqueue vertex.edges (j, vertices.(code).status_line)
    done
  done;
  for i = 0 to max_code do
    Queue.iter vertices.(i).edges ~f:(fun (j, status_line) ->
      let code = Status_line.code status_line in
      Queue.enqueue vertices.(code).reverse_edges (j, vertices.(i).status_line))
  done;
  { vertices }
;;

let edges t ~code =
  if code < 0 || code >= Array.length t.vertices
  then []
  else Queue.to_list t.vertices.(code).edges
;;

module Step = struct
  type t =
    | Open_box of int
    | Status_line of Status_line.t
    | Bunny_moved of Status_line.t
    | Bunny_was_caught
  [@@deriving sexp_of]
end

let execute_sequence t ~sequence =
  let max_code = Array.length t.vertices - 1 in
  let all = t.vertices.(max_code).status_line in
  let rec aux acc code = function
    | [] -> List.rev acc
    | hd :: tl ->
      let status_line = t.vertices.(code).status_line in
      if [%equal: int option] (Some hd) (Status_line.catch_the_bunny status_line)
      then aux Step.(Bunny_was_caught :: Open_box hd :: acc) code []
      else (
        let updated_status_line = Status_line.remove status_line ~index:hd in
        let _, bunny_moved =
          Queue.find t.vertices.(code).edges ~f:(fun (i, _) -> i = hd)
          |> Option.value_exn ~here:[%here]
        in
        aux
          Step.(
            Bunny_moved bunny_moved
            :: Status_line updated_status_line
            :: Open_box hd
            :: acc)
          (Status_line.code bunny_moved)
          tl)
  in
  aux [ Status_line all ] max_code sequence
;;

module Solution = struct
  type t =
    { length : int
    ; sequence : int list
    }
  [@@deriving sexp_of]
end

let catch_the_bunny t =
  let max_code = Array.length t.vertices - 1 in
  let sequences = Queue.create () in
  for i = 0 to Array.length t.vertices - 1 do
    let status_line = t.vertices.(i).status_line in
    match Status_line.catch_the_bunny status_line with
    | None -> ()
    | Some index ->
      (* We do a reverse search from that status_line. Since we know
         there are solutions with less than 15 moves, we stop the
         search at that depth. *)
      let rec search acc visited code =
        let visited = Set.add visited code in
        if code = max_code
        then Queue.enqueue sequences acc
        else if List.length acc < 15
        then
          Queue.iter t.vertices.(code).reverse_edges ~f:(fun (j, status_line) ->
            let code = Status_line.code status_line in
            if not (Set.mem visited code) then search (j :: acc) visited code)
      in
      search [ index ] (Set.empty (module Int)) (Status_line.code status_line)
  done;
  List.map (Queue.to_list sequences) ~f:(fun indexes -> List.length indexes, indexes)
  |> List.sort ~compare:[%compare: int * int list]
  |> List.map ~f:(fun (length, sequence) -> { Solution.length; sequence })
;;
