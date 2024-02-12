let%expect_test "code" =
  for code = 0 to 255 do
    let status_line = Status_line.create ~size:8 ~code in
    let code' = Status_line.code status_line in
    require_equal [%here] (module Int) code code' ~message:"code does not round trip"
  done;
  [%expect {||}];
  require_does_raise [%here] (fun () : Status_line.t ->
    Status_line.create ~size:(-1) ~code:0);
  [%expect {| ("invalid size, expected >= 1" ((size -1))) |}];
  require_does_raise [%here] (fun () : Status_line.t ->
    Status_line.create ~size:0 ~code:0);
  [%expect {| ("invalid size, expected >= 1" ((size 0))) |}];
  require_does_not_raise [%here] (fun () ->
    ignore (Status_line.create ~size:1 ~code:0 : Status_line.t));
  [%expect {||}];
  require_does_raise [%here] (fun () : Status_line.t ->
    Status_line.create ~size:1 ~code:(-1));
  [%expect {|
    ("code out of bounds" (
      (size 1)
      (code -1))) |}];
  require_does_raise [%here] (fun () : Status_line.t ->
    Status_line.create ~size:1 ~code:3);
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
  require_does_raise [%here] (fun () : Status_line.t -> Status_line.remove t ~index:(-1));
  [%expect {|
    ("index out of bounds" (
      (size  8)
      (index -1))) |}];
  require_does_raise [%here] (fun () : Status_line.t -> Status_line.remove t ~index:8);
  [%expect {|
    ("index out of bounds" (
      (size  8)
      (index 8))) |}];
  ()
;;
