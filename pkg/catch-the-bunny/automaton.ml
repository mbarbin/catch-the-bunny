(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module Edge = struct
  type t =
    { target : int
    ; label : Status_line.t
    }

  let compare t { target; label } =
    match Int.compare t.target target with
    | (Lt | Gt) as r -> r
    | Eq -> Status_line.compare t.label label
  ;;

  let to_dyn { target; label } =
    Dyn.record [ "target", target |> Dyn.int; "label", label |> Status_line.to_dyn ]
  ;;
end

module Vertex = struct
  type t =
    { status_line : Status_line.t
    ; edges : Edge.t list
    ; reverse_edges : Edge.t list
    }
end

type t = { vertices : Vertex.t array }

let create ~size =
  let max_code = (1 lsl size) - 1 in
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
        { Edge.target = j; label = status_lines.(code) }))
  in
  let reverse_edges =
    let reverse_edges = Array.init (max_code + 1) ~f:(fun _ -> []) in
    Array.iteri edges ~f:(fun i edges ->
      let origin = status_lines.(i) in
      List.iter edges ~f:(fun { Edge.target = j; label = status_line } ->
        let code = Status_line.code status_line in
        reverse_edges.(code)
        <- { Edge.target = j; label = origin } :: reverse_edges.(code)));
    reverse_edges
  in
  let vertices =
    Array.mapi status_lines ~f:(fun code status_line ->
      let reverse_edges = reverse_edges.(code) |> List.sort (module Edge) in
      { Vertex.status_line; edges = edges.(code); reverse_edges })
  in
  { vertices }
;;

let check_code_exn t ~code =
  let length = Array.length t.vertices in
  if code < 0 || code >= length
  then
    Code_error.raise
      "Code out of bounds."
      [ "code", code |> Dyn.int; "length", length |> Dyn.int ]
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

  let to_dyn = function
    | Open_box i -> Dyn.variant "Open_box" [ Dyn.int i ]
    | Status_line l -> Dyn.variant "Status_line" [ Status_line.to_dyn l ]
    | Bunny_moved l -> Dyn.variant "Bunny_moved" [ Status_line.to_dyn l ]
    | Bunny_was_caught -> Dyn.variant "Bunny_was_caught" []
  ;;
end

let execute_sequence t ~sequence =
  let max_code = Array.length t.vertices - 1 in
  let all = t.vertices.(max_code).status_line in
  let rec aux acc code = function
    | [] -> List.rev acc
    | hd :: tl ->
      let status_line = t.vertices.(code).status_line in
      if Status_line.bunny_was_caught status_line ~index:hd
      then aux Step.(Bunny_was_caught :: Open_box hd :: acc) code []
      else (
        let updated_status_line = Status_line.remove status_line ~index:hd in
        let { Edge.target = _; label = bunny_moved } =
          List.find_exn t.vertices.(code).edges ~f:(fun { Edge.target = i; _ } -> i = hd)
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

  let to_dyn { length; sequence } =
    Dyn.record [ "length", length |> Dyn.int; "sequence", sequence |> Dyn.list Dyn.int ]
  ;;

  let compare t { length; sequence } =
    match Int.compare t.length length with
    | (Lt | Gt) as r -> r
    | Eq -> List.compare (module Int) t.sequence sequence
  ;;
end

let catch_the_bunny t =
  (* [max_code] is the code of the initial state where all bits of
     may_be_located are 1. *)
  let max_code = Array.length t.vertices - 1 in
  let sequences = ref [] in
  Array.iter t.vertices ~f:(fun vertex ->
    let status_line = vertex.status_line in
    match Status_line.may_catch_the_bunny status_line with
    | None -> ()
    | Some index ->
      (* We do a reverse search from that status_line. Since we know there are
         solutions with less than 15 moves, we stop the search at that depth. *)
      let visited = Bitv.create (max_code + 1) false in
      let rec search acc code =
        Bitv.set visited code true;
        search_visiting acc code;
        Bitv.set visited code false
      and search_visiting acc code =
        if code = max_code
        then sequences := acc :: !sequences
        else if List.length acc < 15
        then
          List.iter
            t.vertices.(code).reverse_edges
            ~f:(fun { Edge.target = j; label = status_line } ->
              let code = Status_line.code status_line in
              if not (Bitv.get visited code) then search (j :: acc) code)
      in
      search [ index ] (Status_line.code status_line));
  List.map !sequences ~f:(fun sequence ->
    { Solution.length = List.length sequence; sequence })
  |> List.sort (module Solution)
;;
