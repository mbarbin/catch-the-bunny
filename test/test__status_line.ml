let%expect_test "code" =
  for code = 0 to 255 do
    let status_line = Status_line.create ~size:8 ~code in
    let code' = Status_line.code status_line in
    if code <> code'
    then
      raise_s
        [%sexp "code does not round trip", { code : int; code' : int }] [@coverage off]
  done;
  [%expect {||}];
  require_does_raise [%here] (fun () ->
    ignore (Status_line.create ~size:(-1) ~code:0 : Status_line.t));
  [%expect {| (Invalid_argument "Array.create ~len:-1: invalid length") |}];
  require_does_not_raise [%here] (fun () ->
    ignore (Status_line.create ~size:0 ~code:0 : Status_line.t));
  [%expect {||}];
  require_does_raise [%here] (fun () ->
    ignore (Status_line.create ~size:1 ~code:(-1) : Status_line.t));
  [%expect {|
    ("invalid negative code" (
      (size 1)
      (code -1))) |}];
  require_does_raise [%here] (fun () ->
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
  [%expect {|
    ((code           191)
     (may_be_located 10111111)) |}];
  let t = Status_line.move t in
  print_s [%sexp (t : Status_line.t)];
  [%expect {|
    ((code           127)
     (may_be_located 01111111)) |}];
  ()
;;

let%expect_test "regression" =
  let t = Status_line.create ~size:8 ~code:127 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {|
    ((code           127)
     (may_be_located 01111111)) |}];
  let t = Status_line.remove t ~index:2 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {|
    ((code           95)
     (may_be_located 01011111)) |}];
  let t = Status_line.move t in
  print_s [%sexp (t : Status_line.t)];
  [%expect {|
    ((code           191)
     (may_be_located 10111111)) |}];
  ()
;;

let%expect_test "remove" =
  let t = Status_line.create ~size:8 ~code:127 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {|
    ((code           127)
     (may_be_located 01111111)) |}];
  let t' = Status_line.remove t ~index:0 in
  print_s [%sexp (t' : Status_line.t)];
  [%expect {|
    ((code           127)
     (may_be_located 01111111)) |}];
  require_equal [%here] (module Status_line) t t';
  let t = Status_line.remove t ~index:2 in
  print_s [%sexp (t : Status_line.t)];
  [%expect {|
    ((code           95)
     (may_be_located 01011111)) |}];
  require_does_raise [%here] (fun () ->
    ignore (Status_line.remove t ~index:(-1) : Status_line.t));
  [%expect {|
    ("index out of bounds" (
      (size  8)
      (index -1))) |}];
  require_does_raise [%here] (fun () ->
    ignore (Status_line.remove t ~index:8 : Status_line.t));
  [%expect {|
    ("index out of bounds" (
      (size  8)
      (index 8))) |}];
  ()
;;
