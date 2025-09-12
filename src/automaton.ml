(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module Vertex = struct
  type t =
    { status_line : Status_line.t
    ; edges : (int * Status_line.t) list
    ; reverse_edges : (int * Status_line.t) list
    }
end

type t = { vertices : Vertex.t array }

let create ~size =
  let max_code = Int.pow 2 size - 1 in
  let status_lines =
    Array.init (max_code + 1) ~f:(fun code -> Status_line.create ~size ~code)
  in
  let edges =
    Array.map status_lines ~f:(fun status_line ->
      List.init size ~f:(fun j ->
        let code =
          status_line
          |> Status_line.remove ~index:j
          |> Status_line.move
          |> Status_line.code
        in
        j, status_lines.(code)))
  in
  let reverse_edges =
    let reverse_edges = Array.init (max_code + 1) ~f:(fun _ -> Queue.create ()) in
    Array.iteri edges ~f:(fun i edges ->
      let origin = status_lines.(i) in
      List.iter edges ~f:(fun (j, status_line) ->
        let code = Status_line.code status_line in
        Queue.enqueue reverse_edges.(code) (j, origin)));
    reverse_edges
  in
  let vertices =
    Array.mapi status_lines ~f:(fun code status_line ->
      let reverse_edges =
        reverse_edges.(code)
        |> Queue.to_list
        |> List.sort ~compare:[%compare: int * Status_line.t]
      in
      { Vertex.status_line; edges = edges.(code); reverse_edges })
  in
  { vertices }
;;

let check_code_exn t ~code =
  let length = Array.length t.vertices in
  if code < 0 || code >= length
  then raise_s [%sexp "code out of bounds", { code : int; length : int }]
;;

let edges t ~code =
  check_code_exn t ~code;
  t.vertices.(code).edges
;;

let reverse_edges t ~code =
  check_code_exn t ~code;
  t.vertices.(code).reverse_edges
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
          List.find t.vertices.(code).edges ~f:(fun (i, _) -> i = hd)
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
  Array.iter t.vertices ~f:(fun vertex ->
    let status_line = vertex.status_line in
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
          List.iter t.vertices.(code).reverse_edges ~f:(fun (j, status_line) ->
            let code = Status_line.code status_line in
            if not (Set.mem visited code) then search (j :: acc) visited code)
      in
      search [ index ] (Set.empty (module Int)) (Status_line.code status_line));
  List.map (Queue.to_list sequences) ~f:(fun indexes -> List.length indexes, indexes)
  |> List.sort ~compare:[%compare: int * int list]
  |> List.map ~f:(fun (length, sequence) -> { Solution.length; sequence })
;;
