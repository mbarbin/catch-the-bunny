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
     (2 ((code 255) (may_be_present (true true true true true true true true))))
     (3 ((code 255) (may_be_present (true true true true true true true true))))
     (4 ((code 255) (may_be_present (true true true true true true true true))))
     (5 ((code 253) (may_be_present (true true true true true true false true))))
     (6 ((code 254) (may_be_present (true true true true true true true false))))
     (7 ((code 255) (may_be_present (true true true true true true true true))))) |}];
  ()
;;
