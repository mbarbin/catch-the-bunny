(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "code" =
  for code = 0 to 255 do
    let status_line = Status_line.create ~size:8 ~code in
    Status_line.check_code_does_round_trip_exn status_line ~expected:code
  done;
  [%expect {||}];
  require_does_raise (fun () ->
    Status_line.check_code_does_round_trip_exn
      (Status_line.create ~size:8 ~code:1)
      ~expected:0);
  [%expect {| ("Code does not round trip.", { expected = 0; computed = 1 }) |}];
  require_does_raise (fun () : Status_line.t -> Status_line.create ~size:(-1) ~code:0);
  [%expect {| ("Invalid size, expected [>= 1].", { size = -1 }) |}];
  require_does_raise (fun () : Status_line.t -> Status_line.create ~size:0 ~code:0);
  [%expect {| ("Invalid size, expected [>= 1].", { size = 0 }) |}];
  ignore (Status_line.create ~size:1 ~code:0 : Status_line.t);
  [%expect {||}];
  require_does_raise (fun () : Status_line.t -> Status_line.create ~size:1 ~code:(-1));
  [%expect {| ("Code out of bounds.", { size = 1; code = -1 }) |}];
  require_does_raise (fun () : Status_line.t -> Status_line.create ~size:1 ~code:3);
  [%expect {| ("Code out of bounds.", { size = 1; code = 3 }) |}];
  ()
;;

let%expect_test "move" =
  let t = Status_line.create ~size:8 ~code:255 in
  let t = Status_line.remove t ~index:1 in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 191; may_be_located = "10111111" } |}];
  let t = Status_line.move t in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 127; may_be_located = "01111111" } |}];
  ()
;;

let%expect_test "regression" =
  let t = Status_line.create ~size:8 ~code:127 in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 127; may_be_located = "01111111" } |}];
  let t = Status_line.remove t ~index:2 in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 95; may_be_located = "01011111" } |}];
  let t = Status_line.move t in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 191; may_be_located = "10111111" } |}];
  ()
;;

let%expect_test "remove" =
  let t = Status_line.create ~size:8 ~code:127 in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 127; may_be_located = "01111111" } |}];
  let t' = Status_line.remove t ~index:0 in
  print_dyn (t' |> Status_line.to_dyn);
  [%expect {| { code = 127; may_be_located = "01111111" } |}];
  require_equal (module Status_line) t t';
  let t = Status_line.remove t ~index:2 in
  print_dyn (t |> Status_line.to_dyn);
  [%expect {| { code = 95; may_be_located = "01011111" } |}];
  require_does_raise (fun () : Status_line.t -> Status_line.remove t ~index:(-1));
  [%expect {| ("Index out of bounds.", { size = 8; index = -1 }) |}];
  require_does_raise (fun () : Status_line.t -> Status_line.remove t ~index:8);
  [%expect {| ("Index out of bounds.", { size = 8; index = 8 }) |}];
  ()
;;
