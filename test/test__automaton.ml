open! Core
open! Bunny

let%expect_test "edges" =
  let t = Automaton.create ~length:8 in
  let print_code ~code =
    let edges = Automaton.edges t ~code in
    print_s [%sexp (edges : (int * Status_line.t) list)]
  in
  print_code ~code:255;
  [%expect
    {|
    ((0 ((code 255) (may_be_present (true true true true true true true true))))
     (1 ((code 127) (may_be_present (false true true true true true true true))))
     (2 ((code 255) (may_be_present (true true true true true true true true))))
     (3 ((code 255) (may_be_present (true true true true true true true true))))
     (4 ((code 255) (may_be_present (true true true true true true true true))))
     (5 ((code 255) (may_be_present (true true true true true true true true))))
     (6 ((code 254) (may_be_present (true true true true true true true false))))
     (7 ((code 255) (may_be_present (true true true true true true true true))))) |}];
  print_code ~code:127;
  [%expect
    {|
    ((0 ((code 255) (may_be_present (true true true true true true true true))))
     (1 ((code 127) (may_be_present (false true true true true true true true))))
     (2 ((code 191) (may_be_present (true false true true true true true true))))
     (3 ((code 255) (may_be_present (true true true true true true true true))))
     (4 ((code 255) (may_be_present (true true true true true true true true))))
     (5 ((code 255) (may_be_present (true true true true true true true true))))
     (6 ((code 254) (may_be_present (true true true true true true true false))))
     (7 ((code 255) (may_be_present (true true true true true true true true))))) |}];
  print_code ~code:191;
  [%expect
    {|
    ((0 ((code 127) (may_be_present (false true true true true true true true))))
     (1 ((code 127) (may_be_present (false true true true true true true true))))
     (2 ((code 127) (may_be_present (false true true true true true true true))))
     (3 ((code 95) (may_be_present (false true false true true true true true))))
     (4 ((code 127) (may_be_present (false true true true true true true true))))
     (5 ((code 127) (may_be_present (false true true true true true true true))))
     (6
      ((code 126) (may_be_present (false true true true true true true false))))
     (7 ((code 127) (may_be_present (false true true true true true true true))))) |}];
  ()
;;

let%expect_test "edges" =
  let t = Automaton.create ~length:8 in
  let print_sequence indexes =
    print_s [%sexp (Automaton.sequence t indexes : Automaton.Step.t list)]
  in
  print_sequence [ 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ];
  [%expect
    {|
    ((Status_line
      ((code 255) (may_be_present (true true true true true true true true))))
     (Open_box 1)
     (Status_line
      ((code 191) (may_be_present (true false true true true true true true))))
     (Bunny_moved
      ((code 127) (may_be_present (false true true true true true true true))))
     (Open_box 2)
     (Status_line
      ((code 95) (may_be_present (false true false true true true true true))))
     (Bunny_moved
      ((code 191) (may_be_present (true false true true true true true true))))
     (Open_box 3)
     (Status_line
      ((code 175) (may_be_present (true false true false true true true true))))
     (Bunny_moved
      ((code 95) (may_be_present (false true false true true true true true))))
     (Open_box 4)
     (Status_line
      ((code 87) (may_be_present (false true false true false true true true))))
     (Bunny_moved
      ((code 175) (may_be_present (true false true false true true true true))))
     (Open_box 5)
     (Status_line
      ((code 171) (may_be_present (true false true false true false true true))))
     (Bunny_moved
      ((code 87) (may_be_present (false true false true false true true true))))
     (Open_box 6)
     (Status_line
      ((code 85) (may_be_present (false true false true false true false true))))
     (Bunny_moved
      ((code 170) (may_be_present (true false true false true false true false))))
     (Open_box 6)
     (Status_line
      ((code 168)
       (may_be_present (true false true false true false false false))))
     (Bunny_moved
      ((code 84) (may_be_present (false true false true false true false false))))
     (Open_box 5)
     (Status_line
      ((code 80)
       (may_be_present (false true false true false false false false))))
     (Bunny_moved
      ((code 168)
       (may_be_present (true false true false true false false false))))
     (Open_box 4)
     (Status_line
      ((code 160)
       (may_be_present (true false true false false false false false))))
     (Bunny_moved
      ((code 80)
       (may_be_present (false true false true false false false false))))
     (Open_box 3)
     (Status_line
      ((code 64)
       (may_be_present (false true false false false false false false))))
     (Bunny_moved
      ((code 160)
       (may_be_present (true false true false false false false false))))
     (Open_box 2)
     (Status_line
      ((code 128)
       (may_be_present (true false false false false false false false))))
     (Bunny_moved
      ((code 64)
       (may_be_present (false true false false false false false false))))
     (Open_box 1) Bunny_was_caught) |}];
  ()
;;

let%expect_test "catch it" =
  let t = Automaton.create ~length:8 in
  let sequences = Automaton.catch_bunny t in
  print_s [%sexp (sequences : (int * int list) list list)];
  [%expect
    {|
    (((12 (6 5 4 3 2 1 1 2 3 4 5 6)) (12 (1 2 3 4 5 6 6 5 4 3 2 1)))
     ((13 (1 2 3 4 5 6 6 1 2 3 4 5 6)) (13 (1 2 3 4 5 6 0 1 2 3 4 5 6))
      (13 (1 2 3 4 5 6 1 1 2 3 4 5 6)) (13 (1 2 3 4 5 6 2 1 2 3 4 5 6))
      (13 (1 2 3 4 5 6 3 1 2 3 4 5 6)) (13 (1 2 3 4 5 6 4 1 2 3 4 5 6))
      (13 (1 2 3 4 5 6 5 1 2 3 4 5 6)) (13 (1 2 3 4 5 6 7 1 2 3 4 5 6))
      (13 (1 6 5 4 3 2 1 1 2 3 4 5 6)) (13 (6 5 4 3 2 1 1 6 5 4 3 2 1))
      (13 (6 5 4 3 2 1 0 6 5 4 3 2 1)) (13 (6 5 4 3 2 1 2 6 5 4 3 2 1))
      (13 (6 5 4 3 2 1 3 6 5 4 3 2 1)) (13 (6 5 4 3 2 1 4 6 5 4 3 2 1))
      (13 (6 5 4 3 2 1 5 6 5 4 3 2 1)) (13 (6 5 4 3 2 1 6 6 5 4 3 2 1))
      (13 (6 5 4 3 2 1 7 6 5 4 3 2 1)) (13 (6 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((14 (6 5 4 3 2 1 0 6 1 2 3 4 5 6)) (14 (6 5 4 3 2 1 2 6 1 2 3 4 5 6))
      (14 (6 5 4 3 2 1 3 6 1 2 3 4 5 6)) (14 (6 5 4 3 2 1 4 6 1 2 3 4 5 6))
      (14 (6 5 4 3 2 1 5 6 1 2 3 4 5 6)) (14 (6 5 4 3 2 1 6 6 1 2 3 4 5 6))
      (14 (6 5 4 3 2 1 7 6 1 2 3 4 5 6)) (14 (6 1 2 3 4 5 6 6 1 2 3 4 5 6))
      (14 (6 1 2 3 4 5 6 0 1 2 3 4 5 6)) (14 (6 1 2 3 4 5 6 1 1 2 3 4 5 6))
      (14 (6 1 2 3 4 5 6 2 1 2 3 4 5 6)) (14 (6 1 2 3 4 5 6 3 1 2 3 4 5 6))
      (14 (6 1 2 3 4 5 6 4 1 2 3 4 5 6)) (14 (6 1 2 3 4 5 6 5 1 2 3 4 5 6))
      (14 (6 1 2 3 4 5 6 7 1 2 3 4 5 6)) (14 (1 2 6 5 4 3 2 1 1 2 3 4 5 6))
      (14 (1 2 3 4 5 6 0 1 6 5 4 3 2 1)) (14 (1 2 3 4 5 6 1 1 6 5 4 3 2 1))
      (14 (1 2 3 4 5 6 2 1 6 5 4 3 2 1)) (14 (1 2 3 4 5 6 3 1 6 5 4 3 2 1))
      (14 (1 2 3 4 5 6 4 1 6 5 4 3 2 1)) (14 (1 2 3 4 5 6 5 1 6 5 4 3 2 1))
      (14 (1 2 3 4 5 6 7 1 6 5 4 3 2 1)) (14 (1 6 5 4 3 2 1 1 6 5 4 3 2 1))
      (14 (1 6 5 4 3 2 1 0 6 5 4 3 2 1)) (14 (1 6 5 4 3 2 1 2 6 5 4 3 2 1))
      (14 (1 6 5 4 3 2 1 3 6 5 4 3 2 1)) (14 (1 6 5 4 3 2 1 4 6 5 4 3 2 1))
      (14 (1 6 5 4 3 2 1 5 6 5 4 3 2 1)) (14 (1 6 5 4 3 2 1 6 6 5 4 3 2 1))
      (14 (1 6 5 4 3 2 1 7 6 5 4 3 2 1)) (14 (6 5 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((15 (1 2 3 4 5 6 6 5 4 1 2 3 4 5 6)) (15 (1 6 5 4 3 2 1 0 6 1 2 3 4 5 6))
      (15 (1 6 5 4 3 2 1 2 6 1 2 3 4 5 6)) (15 (1 6 5 4 3 2 1 3 6 1 2 3 4 5 6))
      (15 (1 6 5 4 3 2 1 4 6 1 2 3 4 5 6)) (15 (1 6 5 4 3 2 1 5 6 1 2 3 4 5 6))
      (15 (1 6 5 4 3 2 1 6 6 1 2 3 4 5 6)) (15 (1 6 5 4 3 2 1 7 6 1 2 3 4 5 6))
      (15 (6 5 1 2 3 4 5 6 6 1 2 3 4 5 6)) (15 (6 5 1 2 3 4 5 6 0 1 2 3 4 5 6))
      (15 (6 5 1 2 3 4 5 6 1 1 2 3 4 5 6)) (15 (6 5 1 2 3 4 5 6 2 1 2 3 4 5 6))
      (15 (6 5 1 2 3 4 5 6 3 1 2 3 4 5 6)) (15 (6 5 1 2 3 4 5 6 4 1 2 3 4 5 6))
      (15 (6 5 1 2 3 4 5 6 5 1 2 3 4 5 6)) (15 (6 5 1 2 3 4 5 6 7 1 2 3 4 5 6))
      (15 (1 2 3 6 5 4 3 2 1 1 2 3 4 5 6)) (15 (6 1 2 6 5 4 3 2 1 1 2 3 4 5 6))
      (15 (1 2 6 6 5 4 3 2 1 1 2 3 4 5 6)) (15 (6 5 4 3 2 1 1 2 3 6 5 4 3 2 1))
      (15 (6 1 2 3 4 5 6 0 1 6 5 4 3 2 1)) (15 (6 1 2 3 4 5 6 1 1 6 5 4 3 2 1))
      (15 (6 1 2 3 4 5 6 2 1 6 5 4 3 2 1)) (15 (6 1 2 3 4 5 6 3 1 6 5 4 3 2 1))
      (15 (6 1 2 3 4 5 6 4 1 6 5 4 3 2 1)) (15 (6 1 2 3 4 5 6 5 1 6 5 4 3 2 1))
      (15 (6 1 2 3 4 5 6 7 1 6 5 4 3 2 1)) (15 (1 2 6 5 4 3 2 1 1 6 5 4 3 2 1))
      (15 (1 2 6 5 4 3 2 1 0 6 5 4 3 2 1)) (15 (1 2 6 5 4 3 2 1 2 6 5 4 3 2 1))
      (15 (1 2 6 5 4 3 2 1 3 6 5 4 3 2 1)) (15 (1 2 6 5 4 3 2 1 4 6 5 4 3 2 1))
      (15 (1 2 6 5 4 3 2 1 5 6 5 4 3 2 1)) (15 (1 2 6 5 4 3 2 1 6 6 5 4 3 2 1))
      (15 (1 2 6 5 4 3 2 1 7 6 5 4 3 2 1)) (15 (6 5 4 1 2 3 4 5 6 6 5 4 3 2 1))
      (15 (1 6 5 1 2 3 4 5 6 6 5 4 3 2 1)) (15 (6 5 1 1 2 3 4 5 6 6 5 4 3 2 1)))) |}];
  ()
;;
