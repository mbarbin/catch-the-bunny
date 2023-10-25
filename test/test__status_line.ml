open! Base
open! Stdio
open! Bunny

let%expect_test "code" =
  for code = 0 to 255 do
    let status_line = Status_line.create ~size:8 ~code in
    let code' = Status_line.code status_line in
    if code <> code'
    then raise_s [%sexp "code does not round trip", { code : int; code' : int }]
  done;
  [%expect {| |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    ignore (Status_line.create ~size:(-1) ~code:0 : Status_line.t));
  [%expect {| (Invalid_argument "Array.create ~len:-1: invalid length") |}];
  Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
    ignore (Status_line.create ~size:0 ~code:0 : Status_line.t));
  [%expect {| |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    ignore (Status_line.create ~size:1 ~code:(-1) : Status_line.t));
  [%expect {|
    ("invalid negative code" (
      (size 1)
      (code -1))) |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    ignore (Status_line.create ~size:1 ~code:3 : Status_line.t));
  [%expect {|
    ("code out of bounds" (
      (size 1)
      (code 3))) |}];
  ()
;;

let%expect_test "move" =
  let t = Status_line.create ~size:8 ~code:255 in
  let t = Status_line.remove t ~index:1 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 191) (may_be_located (true false true true true true true true))) |}];
  let t = Status_line.move t in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 127) (may_be_located (false true true true true true true true))) |}];
  ()
;;

let%expect_test "regression" =
  let t = Status_line.create ~size:8 ~code:127 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 127) (may_be_located (false true true true true true true true))) |}];
  let t = Status_line.remove t ~index:2 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 95) (may_be_located (false true false true true true true true))) |}];
  let t = Status_line.move t in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 191) (may_be_located (true false true true true true true true))) |}];
  ()
;;

let%expect_test "remove" =
  let t = Status_line.create ~size:8 ~code:127 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 127) (may_be_located (false true true true true true true true))) |}];
  let t = Status_line.remove t ~index:2 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {| ((code 95) (may_be_located (false true false true true true true true))) |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    ignore (Status_line.remove t ~index:(-1) : Status_line.t));
  [%expect {|
    ("index out of bounds" (
      (size  8)
      (index -1))) |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    ignore (Status_line.remove t ~index:8 : Status_line.t));
  [%expect
    {|
        ("index out of bounds" (
          (size  8)
          (index 8))) |}];
  ()
;;
