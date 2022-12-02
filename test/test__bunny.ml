open! Core

let%expect_test "hello" =
  print_s Bunny.hello_world;
  [%expect {| "Hello, World!" |}]
;;
