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
    ((0 ((code 255) (may_be_located (true true true true true true true true))))
     (1 ((code 127) (may_be_located (false true true true true true true true))))
     (2 ((code 255) (may_be_located (true true true true true true true true))))
     (3 ((code 255) (may_be_located (true true true true true true true true))))
     (4 ((code 255) (may_be_located (true true true true true true true true))))
     (5 ((code 255) (may_be_located (true true true true true true true true))))
     (6 ((code 254) (may_be_located (true true true true true true true false))))
     (7 ((code 255) (may_be_located (true true true true true true true true))))) |}];
  print_code ~code:127;
  [%expect
    {|
    ((0 ((code 255) (may_be_located (true true true true true true true true))))
     (1 ((code 127) (may_be_located (false true true true true true true true))))
     (2 ((code 191) (may_be_located (true false true true true true true true))))
     (3 ((code 255) (may_be_located (true true true true true true true true))))
     (4 ((code 255) (may_be_located (true true true true true true true true))))
     (5 ((code 255) (may_be_located (true true true true true true true true))))
     (6 ((code 254) (may_be_located (true true true true true true true false))))
     (7 ((code 255) (may_be_located (true true true true true true true true))))) |}];
  print_code ~code:191;
  [%expect
    {|
    ((0 ((code 127) (may_be_located (false true true true true true true true))))
     (1 ((code 127) (may_be_located (false true true true true true true true))))
     (2 ((code 127) (may_be_located (false true true true true true true true))))
     (3 ((code 95) (may_be_located (false true false true true true true true))))
     (4 ((code 127) (may_be_located (false true true true true true true true))))
     (5 ((code 127) (may_be_located (false true true true true true true true))))
     (6
      ((code 126) (may_be_located (false true true true true true true false))))
     (7 ((code 127) (may_be_located (false true true true true true true true))))) |}];
  ()
;;

let%expect_test "edges" =
  let t = Automaton.create ~length:8 in
  let print_sequence sequence =
    print_s [%sexp (Automaton.execute_sequence t ~sequence : Automaton.Step.t list)]
  in
  print_sequence [ 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ];
  [%expect
    {|
    ((Status_line
      ((code 255) (may_be_located (true true true true true true true true))))
     (Open_box 1)
     (Status_line
      ((code 191) (may_be_located (true false true true true true true true))))
     (Bunny_moved
      ((code 127) (may_be_located (false true true true true true true true))))
     (Open_box 2)
     (Status_line
      ((code 95) (may_be_located (false true false true true true true true))))
     (Bunny_moved
      ((code 191) (may_be_located (true false true true true true true true))))
     (Open_box 3)
     (Status_line
      ((code 175) (may_be_located (true false true false true true true true))))
     (Bunny_moved
      ((code 95) (may_be_located (false true false true true true true true))))
     (Open_box 4)
     (Status_line
      ((code 87) (may_be_located (false true false true false true true true))))
     (Bunny_moved
      ((code 175) (may_be_located (true false true false true true true true))))
     (Open_box 5)
     (Status_line
      ((code 171) (may_be_located (true false true false true false true true))))
     (Bunny_moved
      ((code 87) (may_be_located (false true false true false true true true))))
     (Open_box 6)
     (Status_line
      ((code 85) (may_be_located (false true false true false true false true))))
     (Bunny_moved
      ((code 170) (may_be_located (true false true false true false true false))))
     (Open_box 6)
     (Status_line
      ((code 168)
       (may_be_located (true false true false true false false false))))
     (Bunny_moved
      ((code 84) (may_be_located (false true false true false true false false))))
     (Open_box 5)
     (Status_line
      ((code 80)
       (may_be_located (false true false true false false false false))))
     (Bunny_moved
      ((code 168)
       (may_be_located (true false true false true false false false))))
     (Open_box 4)
     (Status_line
      ((code 160)
       (may_be_located (true false true false false false false false))))
     (Bunny_moved
      ((code 80)
       (may_be_located (false true false true false false false false))))
     (Open_box 3)
     (Status_line
      ((code 64)
       (may_be_located (false true false false false false false false))))
     (Bunny_moved
      ((code 160)
       (may_be_located (true false true false false false false false))))
     (Open_box 2)
     (Status_line
      ((code 128)
       (may_be_located (true false false false false false false false))))
     (Bunny_moved
      ((code 64)
       (may_be_located (false true false false false false false false))))
     (Open_box 1) Bunny_was_caught) |}];
  ()
;;

let%expect_test "catch it" =
  let t = Automaton.create ~length:8 in
  let solutions = Automaton.catch_the_bunny t in
  print_s [%sexp (solutions : Automaton.Solution.t list)];
  [%expect
    {|
    (((length 12) (sequence (1 2 3 4 5 6 6 5 4 3 2 1)))
     ((length 12) (sequence (6 5 4 3 2 1 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 0 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 1 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 2 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 3 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 4 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 5 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 6 1 2 3 4 5 6)))
     ((length 13) (sequence (1 2 3 4 5 6 7 1 2 3 4 5 6)))
     ((length 13) (sequence (1 6 5 4 3 2 1 1 2 3 4 5 6)))
     ((length 13) (sequence (6 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 0 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 1 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 2 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 3 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 4 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 5 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 6 6 5 4 3 2 1)))
     ((length 13) (sequence (6 5 4 3 2 1 7 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 0 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 1 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 2 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 3 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 4 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 5 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 3 4 5 6 7 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 2 6 5 4 3 2 1 1 2 3 4 5 6)))
     ((length 14) (sequence (1 6 5 4 3 2 1 0 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 1 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 2 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 3 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 4 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 5 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 6 6 5 4 3 2 1)))
     ((length 14) (sequence (1 6 5 4 3 2 1 7 6 5 4 3 2 1)))
     ((length 14) (sequence (6 1 2 3 4 5 6 0 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 1 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 2 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 3 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 4 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 5 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 1 2 3 4 5 6 7 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((length 14) (sequence (6 5 4 3 2 1 0 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 4 3 2 1 2 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 4 3 2 1 3 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 4 3 2 1 4 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 4 3 2 1 5 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 4 3 2 1 6 6 1 2 3 4 5 6)))
     ((length 14) (sequence (6 5 4 3 2 1 7 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 2 3 4 5 6 6 5 4 1 2 3 4 5 6)))
     ((length 15) (sequence (1 2 3 6 5 4 3 2 1 1 2 3 4 5 6)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 0 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 1 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 2 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 3 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 4 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 5 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 6 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 5 4 3 2 1 7 6 5 4 3 2 1)))
     ((length 15) (sequence (1 2 6 6 5 4 3 2 1 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((length 15) (sequence (1 6 5 4 3 2 1 0 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 4 3 2 1 2 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 4 3 2 1 3 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 4 3 2 1 4 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 4 3 2 1 5 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 4 3 2 1 6 6 1 2 3 4 5 6)))
     ((length 15) (sequence (1 6 5 4 3 2 1 7 6 1 2 3 4 5 6)))
     ((length 15) (sequence (6 1 2 3 4 5 6 0 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 3 4 5 6 1 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 3 4 5 6 2 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 3 4 5 6 3 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 3 4 5 6 4 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 3 4 5 6 5 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 3 4 5 6 7 1 6 5 4 3 2 1)))
     ((length 15) (sequence (6 1 2 6 5 4 3 2 1 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 0 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 1 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 2 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 3 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 4 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 5 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 6 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 1 2 3 4 5 6 7 1 2 3 4 5 6)))
     ((length 15) (sequence (6 5 4 1 2 3 4 5 6 6 5 4 3 2 1)))
     ((length 15) (sequence (6 5 4 3 2 1 1 2 3 6 5 4 3 2 1)))) |}];
  ()
;;

let%expect_test "solve size 6" =
  let t = Automaton.create ~length:6 in
  let solutions = Automaton.catch_the_bunny t in
  print_s [%sexp (solutions : Automaton.Solution.t list)];
  [%expect
    {|
    (((length 8) (sequence (1 2 3 4 4 3 2 1)))
     ((length 8) (sequence (4 3 2 1 1 2 3 4)))
     ((length 9) (sequence (1 2 3 4 0 1 2 3 4)))
     ((length 9) (sequence (1 2 3 4 1 1 2 3 4)))
     ((length 9) (sequence (1 2 3 4 2 1 2 3 4)))
     ((length 9) (sequence (1 2 3 4 3 1 2 3 4)))
     ((length 9) (sequence (1 2 3 4 4 1 2 3 4)))
     ((length 9) (sequence (1 2 3 4 5 1 2 3 4)))
     ((length 9) (sequence (1 4 3 2 1 1 2 3 4)))
     ((length 9) (sequence (4 1 2 3 4 4 3 2 1)))
     ((length 9) (sequence (4 3 2 1 0 4 3 2 1)))
     ((length 9) (sequence (4 3 2 1 1 4 3 2 1)))
     ((length 9) (sequence (4 3 2 1 2 4 3 2 1)))
     ((length 9) (sequence (4 3 2 1 3 4 3 2 1)))
     ((length 9) (sequence (4 3 2 1 4 4 3 2 1)))
     ((length 9) (sequence (4 3 2 1 5 4 3 2 1)))
     ((length 10) (sequence (1 2 3 4 0 1 4 3 2 1)))
     ((length 10) (sequence (1 2 3 4 1 1 4 3 2 1)))
     ((length 10) (sequence (1 2 3 4 2 1 4 3 2 1)))
     ((length 10) (sequence (1 2 3 4 3 1 4 3 2 1)))
     ((length 10) (sequence (1 2 3 4 5 1 4 3 2 1)))
     ((length 10) (sequence (1 2 4 3 2 1 1 2 3 4)))
     ((length 10) (sequence (1 4 3 2 1 0 4 3 2 1)))
     ((length 10) (sequence (1 4 3 2 1 1 4 3 2 1)))
     ((length 10) (sequence (1 4 3 2 1 2 4 3 2 1)))
     ((length 10) (sequence (1 4 3 2 1 3 4 3 2 1)))
     ((length 10) (sequence (1 4 3 2 1 4 4 3 2 1)))
     ((length 10) (sequence (1 4 3 2 1 5 4 3 2 1)))
     ((length 10) (sequence (4 1 2 3 4 0 1 2 3 4)))
     ((length 10) (sequence (4 1 2 3 4 1 1 2 3 4)))
     ((length 10) (sequence (4 1 2 3 4 2 1 2 3 4)))
     ((length 10) (sequence (4 1 2 3 4 3 1 2 3 4)))
     ((length 10) (sequence (4 1 2 3 4 4 1 2 3 4)))
     ((length 10) (sequence (4 1 2 3 4 5 1 2 3 4)))
     ((length 10) (sequence (4 3 1 2 3 4 4 3 2 1)))
     ((length 10) (sequence (4 3 2 1 0 4 1 2 3 4)))
     ((length 10) (sequence (4 3 2 1 2 4 1 2 3 4)))
     ((length 10) (sequence (4 3 2 1 3 4 1 2 3 4)))
     ((length 10) (sequence (4 3 2 1 4 4 1 2 3 4)))
     ((length 10) (sequence (4 3 2 1 5 4 1 2 3 4)))
     ((length 11) (sequence (1 2 4 3 2 1 0 4 3 2 1)))
     ((length 11) (sequence (1 2 4 3 2 1 1 4 3 2 1)))
     ((length 11) (sequence (1 2 4 3 2 1 2 4 3 2 1)))
     ((length 11) (sequence (1 2 4 3 2 1 3 4 3 2 1)))
     ((length 11) (sequence (1 2 4 3 2 1 4 4 3 2 1)))
     ((length 11) (sequence (1 2 4 3 2 1 5 4 3 2 1)))
     ((length 11) (sequence (1 2 4 4 3 2 1 1 2 3 4)))
     ((length 11) (sequence (1 4 3 1 2 3 4 4 3 2 1)))
     ((length 11) (sequence (1 4 3 2 1 0 4 1 2 3 4)))
     ((length 11) (sequence (1 4 3 2 1 2 4 1 2 3 4)))
     ((length 11) (sequence (1 4 3 2 1 3 4 1 2 3 4)))
     ((length 11) (sequence (1 4 3 2 1 4 4 1 2 3 4)))
     ((length 11) (sequence (1 4 3 2 1 5 4 1 2 3 4)))
     ((length 11) (sequence (4 1 2 3 4 0 1 4 3 2 1)))
     ((length 11) (sequence (4 1 2 3 4 1 1 4 3 2 1)))
     ((length 11) (sequence (4 1 2 3 4 2 1 4 3 2 1)))
     ((length 11) (sequence (4 1 2 3 4 3 1 4 3 2 1)))
     ((length 11) (sequence (4 1 2 3 4 5 1 4 3 2 1)))
     ((length 11) (sequence (4 1 2 4 3 2 1 1 2 3 4)))
     ((length 11) (sequence (4 3 1 1 2 3 4 4 3 2 1)))
     ((length 11) (sequence (4 3 1 2 3 4 0 1 2 3 4)))
     ((length 11) (sequence (4 3 1 2 3 4 1 1 2 3 4)))
     ((length 11) (sequence (4 3 1 2 3 4 2 1 2 3 4)))
     ((length 11) (sequence (4 3 1 2 3 4 3 1 2 3 4)))
     ((length 11) (sequence (4 3 1 2 3 4 4 1 2 3 4)))
     ((length 11) (sequence (4 3 1 2 3 4 5 1 2 3 4)))
     ((length 12) (sequence (1 2 3 1 4 3 2 1 1 2 3 4)))
     ((length 12) (sequence (1 2 4 3 2 1 0 4 1 2 3 4)))
     ((length 12) (sequence (1 2 4 3 2 1 2 4 1 2 3 4)))
     ((length 12) (sequence (1 2 4 3 2 1 3 4 1 2 3 4)))
     ((length 12) (sequence (1 2 4 3 2 1 4 4 1 2 3 4)))
     ((length 12) (sequence (1 2 4 3 2 1 5 4 1 2 3 4)))
     ((length 12) (sequence (1 2 4 4 3 2 1 0 4 3 2 1)))
     ((length 12) (sequence (1 2 4 4 3 2 1 1 4 3 2 1)))
     ((length 12) (sequence (1 2 4 4 3 2 1 2 4 3 2 1)))
     ((length 12) (sequence (1 2 4 4 3 2 1 3 4 3 2 1)))
     ((length 12) (sequence (1 2 4 4 3 2 1 4 4 3 2 1)))
     ((length 12) (sequence (1 2 4 4 3 2 1 5 4 3 2 1)))
     ((length 12) (sequence (1 4 3 1 2 3 4 0 1 2 3 4)))
     ((length 12) (sequence (1 4 3 1 2 3 4 1 1 2 3 4)))
     ((length 12) (sequence (1 4 3 1 2 3 4 2 1 2 3 4)))
     ((length 12) (sequence (1 4 3 1 2 3 4 3 1 2 3 4)))
     ((length 12) (sequence (1 4 3 1 2 3 4 4 1 2 3 4)))
     ((length 12) (sequence (1 4 3 1 2 3 4 5 1 2 3 4)))
     ((length 12) (sequence (4 1 2 4 3 2 1 0 4 3 2 1)))
     ((length 12) (sequence (4 1 2 4 3 2 1 1 4 3 2 1)))
     ((length 12) (sequence (4 1 2 4 3 2 1 2 4 3 2 1)))
     ((length 12) (sequence (4 1 2 4 3 2 1 3 4 3 2 1)))
     ((length 12) (sequence (4 1 2 4 3 2 1 4 4 3 2 1)))
     ((length 12) (sequence (4 1 2 4 3 2 1 5 4 3 2 1)))
     ((length 12) (sequence (4 3 1 1 2 3 4 0 1 2 3 4)))
     ((length 12) (sequence (4 3 1 1 2 3 4 1 1 2 3 4)))
     ((length 12) (sequence (4 3 1 1 2 3 4 2 1 2 3 4)))
     ((length 12) (sequence (4 3 1 1 2 3 4 3 1 2 3 4)))
     ((length 12) (sequence (4 3 1 1 2 3 4 4 1 2 3 4)))
     ((length 12) (sequence (4 3 1 1 2 3 4 5 1 2 3 4)))
     ((length 12) (sequence (4 3 1 2 3 4 0 1 4 3 2 1)))
     ((length 12) (sequence (4 3 1 2 3 4 1 1 4 3 2 1)))
     ((length 12) (sequence (4 3 1 2 3 4 2 1 4 3 2 1)))
     ((length 12) (sequence (4 3 1 2 3 4 3 1 4 3 2 1)))
     ((length 12) (sequence (4 3 1 2 3 4 5 1 4 3 2 1)))
     ((length 12) (sequence (4 3 2 4 1 2 3 4 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 3 2 1 0 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 3 2 1 1 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 3 2 1 2 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 3 2 1 3 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 3 2 1 4 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 3 2 1 5 4 3 2 1)))
     ((length 13) (sequence (1 2 3 1 4 4 3 2 1 1 2 3 4)))
     ((length 13) (sequence (1 2 4 4 3 2 1 0 4 1 2 3 4)))
     ((length 13) (sequence (1 2 4 4 3 2 1 2 4 1 2 3 4)))
     ((length 13) (sequence (1 2 4 4 3 2 1 3 4 1 2 3 4)))
     ((length 13) (sequence (1 2 4 4 3 2 1 4 4 1 2 3 4)))
     ((length 13) (sequence (1 2 4 4 3 2 1 5 4 1 2 3 4)))
     ((length 13) (sequence (1 4 3 1 2 3 4 0 1 4 3 2 1)))
     ((length 13) (sequence (1 4 3 1 2 3 4 1 1 4 3 2 1)))
     ((length 13) (sequence (1 4 3 1 2 3 4 2 1 4 3 2 1)))
     ((length 13) (sequence (1 4 3 1 2 3 4 3 1 4 3 2 1)))
     ((length 13) (sequence (1 4 3 1 2 3 4 5 1 4 3 2 1)))
     ((length 13) (sequence (1 4 3 2 4 1 2 3 4 4 3 2 1)))
     ((length 13) (sequence (4 1 2 3 1 4 3 2 1 1 2 3 4)))
     ((length 13) (sequence (4 1 2 4 3 2 1 0 4 1 2 3 4)))
     ((length 13) (sequence (4 1 2 4 3 2 1 2 4 1 2 3 4)))
     ((length 13) (sequence (4 1 2 4 3 2 1 3 4 1 2 3 4)))
     ((length 13) (sequence (4 1 2 4 3 2 1 4 4 1 2 3 4)))
     ((length 13) (sequence (4 1 2 4 3 2 1 5 4 1 2 3 4)))
     ((length 13) (sequence (4 3 1 1 2 3 4 0 1 4 3 2 1)))
     ((length 13) (sequence (4 3 1 1 2 3 4 1 1 4 3 2 1)))
     ((length 13) (sequence (4 3 1 1 2 3 4 2 1 4 3 2 1)))
     ((length 13) (sequence (4 3 1 1 2 3 4 3 1 4 3 2 1)))
     ((length 13) (sequence (4 3 1 1 2 3 4 5 1 4 3 2 1)))
     ((length 13) (sequence (4 3 2 4 1 1 2 3 4 4 3 2 1)))
     ((length 13) (sequence (4 3 2 4 1 2 3 4 0 1 2 3 4)))
     ((length 13) (sequence (4 3 2 4 1 2 3 4 1 1 2 3 4)))
     ((length 13) (sequence (4 3 2 4 1 2 3 4 2 1 2 3 4)))
     ((length 13) (sequence (4 3 2 4 1 2 3 4 3 1 2 3 4)))
     ((length 13) (sequence (4 3 2 4 1 2 3 4 4 1 2 3 4)))
     ((length 13) (sequence (4 3 2 4 1 2 3 4 5 1 2 3 4)))
     ((length 14) (sequence (1 2 3 1 4 3 2 1 0 4 1 2 3 4)))
     ((length 14) (sequence (1 2 3 1 4 3 2 1 2 4 1 2 3 4)))
     ((length 14) (sequence (1 2 3 1 4 3 2 1 3 4 1 2 3 4)))
     ((length 14) (sequence (1 2 3 1 4 3 2 1 4 4 1 2 3 4)))
     ((length 14) (sequence (1 2 3 1 4 3 2 1 5 4 1 2 3 4)))
     ((length 14) (sequence (1 2 3 1 4 4 3 2 1 0 4 3 2 1)))
     ((length 14) (sequence (1 2 3 1 4 4 3 2 1 1 4 3 2 1)))
     ((length 14) (sequence (1 2 3 1 4 4 3 2 1 2 4 3 2 1)))
     ((length 14) (sequence (1 2 3 1 4 4 3 2 1 3 4 3 2 1)))
     ((length 14) (sequence (1 2 3 1 4 4 3 2 1 4 4 3 2 1)))
     ((length 14) (sequence (1 2 3 1 4 4 3 2 1 5 4 3 2 1)))
     ((length 14) (sequence (1 4 3 2 4 1 2 3 4 0 1 2 3 4)))
     ((length 14) (sequence (1 4 3 2 4 1 2 3 4 1 1 2 3 4)))
     ((length 14) (sequence (1 4 3 2 4 1 2 3 4 2 1 2 3 4)))
     ((length 14) (sequence (1 4 3 2 4 1 2 3 4 3 1 2 3 4)))
     ((length 14) (sequence (1 4 3 2 4 1 2 3 4 4 1 2 3 4)))
     ((length 14) (sequence (1 4 3 2 4 1 2 3 4 5 1 2 3 4)))
     ((length 14) (sequence (4 1 2 3 1 4 3 2 1 0 4 3 2 1)))
     ((length 14) (sequence (4 1 2 3 1 4 3 2 1 1 4 3 2 1)))
     ((length 14) (sequence (4 1 2 3 1 4 3 2 1 2 4 3 2 1)))
     ((length 14) (sequence (4 1 2 3 1 4 3 2 1 3 4 3 2 1)))
     ((length 14) (sequence (4 1 2 3 1 4 3 2 1 4 4 3 2 1)))
     ((length 14) (sequence (4 1 2 3 1 4 3 2 1 5 4 3 2 1)))
     ((length 14) (sequence (4 3 2 4 1 1 2 3 4 0 1 2 3 4)))
     ((length 14) (sequence (4 3 2 4 1 1 2 3 4 1 1 2 3 4)))
     ((length 14) (sequence (4 3 2 4 1 1 2 3 4 2 1 2 3 4)))
     ((length 14) (sequence (4 3 2 4 1 1 2 3 4 3 1 2 3 4)))
     ((length 14) (sequence (4 3 2 4 1 1 2 3 4 4 1 2 3 4)))
     ((length 14) (sequence (4 3 2 4 1 1 2 3 4 5 1 2 3 4)))
     ((length 14) (sequence (4 3 2 4 1 2 3 4 0 1 4 3 2 1)))
     ((length 14) (sequence (4 3 2 4 1 2 3 4 1 1 4 3 2 1)))
     ((length 14) (sequence (4 3 2 4 1 2 3 4 2 1 4 3 2 1)))
     ((length 14) (sequence (4 3 2 4 1 2 3 4 3 1 4 3 2 1)))
     ((length 14) (sequence (4 3 2 4 1 2 3 4 5 1 4 3 2 1)))
     ((length 15) (sequence (1 2 3 1 4 4 3 2 1 0 4 1 2 3 4)))
     ((length 15) (sequence (1 2 3 1 4 4 3 2 1 2 4 1 2 3 4)))
     ((length 15) (sequence (1 2 3 1 4 4 3 2 1 3 4 1 2 3 4)))
     ((length 15) (sequence (1 2 3 1 4 4 3 2 1 4 4 1 2 3 4)))
     ((length 15) (sequence (1 2 3 1 4 4 3 2 1 5 4 1 2 3 4)))
     ((length 15) (sequence (1 4 3 2 4 1 2 3 4 0 1 4 3 2 1)))
     ((length 15) (sequence (1 4 3 2 4 1 2 3 4 1 1 4 3 2 1)))
     ((length 15) (sequence (1 4 3 2 4 1 2 3 4 2 1 4 3 2 1)))
     ((length 15) (sequence (1 4 3 2 4 1 2 3 4 3 1 4 3 2 1)))
     ((length 15) (sequence (1 4 3 2 4 1 2 3 4 5 1 4 3 2 1)))
     ((length 15) (sequence (4 1 2 3 1 4 3 2 1 0 4 1 2 3 4)))
     ((length 15) (sequence (4 1 2 3 1 4 3 2 1 2 4 1 2 3 4)))
     ((length 15) (sequence (4 1 2 3 1 4 3 2 1 3 4 1 2 3 4)))
     ((length 15) (sequence (4 1 2 3 1 4 3 2 1 4 4 1 2 3 4)))
     ((length 15) (sequence (4 1 2 3 1 4 3 2 1 5 4 1 2 3 4)))
     ((length 15) (sequence (4 3 2 4 1 1 2 3 4 0 1 4 3 2 1)))
     ((length 15) (sequence (4 3 2 4 1 1 2 3 4 1 1 4 3 2 1)))
     ((length 15) (sequence (4 3 2 4 1 1 2 3 4 2 1 4 3 2 1)))
     ((length 15) (sequence (4 3 2 4 1 1 2 3 4 3 1 4 3 2 1)))
     ((length 15) (sequence (4 3 2 4 1 1 2 3 4 5 1 4 3 2 1)))) |}];
  ()
;;

let%expect_test "show sequence with size 6" =
  let t = Automaton.create ~length:6 in
  let print_sequence sequence =
    print_s [%sexp (Automaton.execute_sequence t ~sequence : Automaton.Step.t list)]
  in
  print_sequence [ 1; 2; 3; 4; 4; 3; 2; 1 ];
  [%expect
    {|
    ((Status_line ((code 63) (may_be_located (true true true true true true))))
     (Open_box 1)
     (Status_line ((code 47) (may_be_located (true false true true true true))))
     (Bunny_moved ((code 31) (may_be_located (false true true true true true))))
     (Open_box 2)
     (Status_line ((code 23) (may_be_located (false true false true true true))))
     (Bunny_moved ((code 47) (may_be_located (true false true true true true))))
     (Open_box 3)
     (Status_line ((code 43) (may_be_located (true false true false true true))))
     (Bunny_moved ((code 23) (may_be_located (false true false true true true))))
     (Open_box 4)
     (Status_line
      ((code 21) (may_be_located (false true false true false true))))
     (Bunny_moved
      ((code 42) (may_be_located (true false true false true false))))
     (Open_box 4)
     (Status_line
      ((code 40) (may_be_located (true false true false false false))))
     (Bunny_moved
      ((code 20) (may_be_located (false true false true false false))))
     (Open_box 3)
     (Status_line
      ((code 16) (may_be_located (false true false false false false))))
     (Bunny_moved
      ((code 40) (may_be_located (true false true false false false))))
     (Open_box 2)
     (Status_line
      ((code 32) (may_be_located (true false false false false false))))
     (Bunny_moved
      ((code 16) (may_be_located (false true false false false false))))
     (Open_box 1) Bunny_was_caught) |}];
  ()
;;
