(*********************************************************************************)
(*  catch-the-bunny: Resolving a small logic puzzle to catch a bunny             *)
(*  SPDX-FileCopyrightText: 2022-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "edges size 8" =
  let t = Automaton.create ~size:8 in
  let print_edges ~code =
    let edges = Automaton.edges t ~code in
    Dyn.print (edges |> Dyn.list Automaton.Edge.to_dyn)
  in
  print_edges ~code:255;
  [%expect
    {|
    [ { target = 0; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 1; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 2; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 3; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 4; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 5; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 6; label = { code = 254; may_be_located = "11111110" } }
    ; { target = 7; label = { code = 255; may_be_located = "11111111" } }
    ]
    |}];
  print_edges ~code:127;
  [%expect
    {|
    [ { target = 0; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 1; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 2; label = { code = 191; may_be_located = "10111111" } }
    ; { target = 3; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 4; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 5; label = { code = 255; may_be_located = "11111111" } }
    ; { target = 6; label = { code = 254; may_be_located = "11111110" } }
    ; { target = 7; label = { code = 255; may_be_located = "11111111" } }
    ]
    |}];
  print_edges ~code:191;
  [%expect
    {|
    [ { target = 0; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 1; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 2; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 3; label = { code = 95; may_be_located = "01011111" } }
    ; { target = 4; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 5; label = { code = 127; may_be_located = "01111111" } }
    ; { target = 6; label = { code = 126; may_be_located = "01111110" } }
    ; { target = 7; label = { code = 127; may_be_located = "01111111" } }
    ]
    |}];
  ()
;;

let%expect_test "sequence size 8" =
  let t = Automaton.create ~size:8 in
  let print_sequence sequence =
    Dyn.print (Automaton.execute_sequence t ~sequence |> Dyn.list Automaton.Step.to_dyn)
  in
  print_sequence [ 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ];
  [%expect
    {|
    [ Status_line { code = 255; may_be_located = "11111111" }
    ; Open_box 1
    ; Status_line { code = 191; may_be_located = "10111111" }
    ; Bunny_moved { code = 127; may_be_located = "01111111" }
    ; Open_box 2
    ; Status_line { code = 95; may_be_located = "01011111" }
    ; Bunny_moved { code = 191; may_be_located = "10111111" }
    ; Open_box 3
    ; Status_line { code = 175; may_be_located = "10101111" }
    ; Bunny_moved { code = 95; may_be_located = "01011111" }
    ; Open_box 4
    ; Status_line { code = 87; may_be_located = "01010111" }
    ; Bunny_moved { code = 175; may_be_located = "10101111" }
    ; Open_box 5
    ; Status_line { code = 171; may_be_located = "10101011" }
    ; Bunny_moved { code = 87; may_be_located = "01010111" }
    ; Open_box 6
    ; Status_line { code = 85; may_be_located = "01010101" }
    ; Bunny_moved { code = 170; may_be_located = "10101010" }
    ; Open_box 6
    ; Status_line { code = 168; may_be_located = "10101000" }
    ; Bunny_moved { code = 84; may_be_located = "01010100" }
    ; Open_box 5
    ; Status_line { code = 80; may_be_located = "01010000" }
    ; Bunny_moved { code = 168; may_be_located = "10101000" }
    ; Open_box 4
    ; Status_line { code = 160; may_be_located = "10100000" }
    ; Bunny_moved { code = 80; may_be_located = "01010000" }
    ; Open_box 3
    ; Status_line { code = 64; may_be_located = "01000000" }
    ; Bunny_moved { code = 160; may_be_located = "10100000" }
    ; Open_box 2
    ; Status_line { code = 128; may_be_located = "10000000" }
    ; Bunny_moved { code = 64; may_be_located = "01000000" }
    ; Open_box 1
    ; Bunny_was_caught
    ]
    |}];
  ()
;;

let%expect_test "solve size 8" =
  let t = Automaton.create ~size:8 in
  let solutions = Automaton.catch_the_bunny t in
  Dyn.print (solutions |> Dyn.list Automaton.Solution.to_dyn);
  [%expect
    {|
    [ { length = 12; sequence = [ 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 6; 5; 4; 3; 2; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 0; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 2; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 3; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 4; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 5; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 4; 5; 6; 7; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 1; 6; 5; 4; 3; 2; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 13; sequence = [ 6; 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 0; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 2; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 3; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 4; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 5; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 6; 5; 4; 3; 2; 1; 7; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 0; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 1; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 2; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 3; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 4; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 5; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 4; 5; 6; 7; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 0; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 2; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 3; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 4; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 5; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 6; 5; 4; 3; 2; 1; 7; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 0; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 2; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 3; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 4; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 5; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 1; 2; 3; 4; 5; 6; 7; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 0; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 2; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 3; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 4; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 5; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 6; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 14; sequence = [ 6; 5; 4; 3; 2; 1; 7; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 4; 5; 6; 6; 5; 4; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 6; 5; 4; 3; 2; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 0; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 2; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 3; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 4; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 5; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 5; 4; 3; 2; 1; 7; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 6; 6; 5; 4; 3; 2; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 0; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 2; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 3; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 4; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 5; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 6; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 1; 6; 5; 4; 3; 2; 1; 7; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 0; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 1; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 2; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 3; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 4; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 5; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 3; 4; 5; 6; 7; 1; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 1; 2; 6; 5; 4; 3; 2; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 0; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 1; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 2; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 3; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 4; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 5; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 6; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 1; 2; 3; 4; 5; 6; 7; 1; 2; 3; 4; 5; 6 ] }
    ; { length = 15; sequence = [ 6; 5; 4; 1; 2; 3; 4; 5; 6; 6; 5; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 6; 5; 4; 3; 2; 1; 1; 2; 3; 6; 5; 4; 3; 2; 1 ] }
    ]
    |}];
  ()
;;

let%expect_test "edges size 6" =
  let t = Automaton.create ~size:6 in
  let print_edges ~code =
    let edges = Automaton.edges t ~code in
    Dyn.print (edges |> Dyn.list Automaton.Edge.to_dyn)
  in
  let print_reverse_edges ~code =
    let reverse_edges = Automaton.reverse_edges t ~code in
    Dyn.print (reverse_edges |> Dyn.list Automaton.Edge.to_dyn)
  in
  print_edges ~code:63;
  [%expect
    {|
    [ { target = 0; label = { code = 63; may_be_located = "111111" } }
    ; { target = 1; label = { code = 31; may_be_located = "011111" } }
    ; { target = 2; label = { code = 63; may_be_located = "111111" } }
    ; { target = 3; label = { code = 63; may_be_located = "111111" } }
    ; { target = 4; label = { code = 62; may_be_located = "111110" } }
    ; { target = 5; label = { code = 63; may_be_located = "111111" } }
    ]
    |}];
  print_edges ~code:31;
  [%expect
    {|
    [ { target = 0; label = { code = 63; may_be_located = "111111" } }
    ; { target = 1; label = { code = 31; may_be_located = "011111" } }
    ; { target = 2; label = { code = 47; may_be_located = "101111" } }
    ; { target = 3; label = { code = 63; may_be_located = "111111" } }
    ; { target = 4; label = { code = 62; may_be_located = "111110" } }
    ; { target = 5; label = { code = 63; may_be_located = "111111" } }
    ]
    |}];
  print_edges ~code:47;
  [%expect
    {|
    [ { target = 0; label = { code = 31; may_be_located = "011111" } }
    ; { target = 1; label = { code = 31; may_be_located = "011111" } }
    ; { target = 2; label = { code = 31; may_be_located = "011111" } }
    ; { target = 3; label = { code = 23; may_be_located = "010111" } }
    ; { target = 4; label = { code = 30; may_be_located = "011110" } }
    ; { target = 5; label = { code = 31; may_be_located = "011111" } }
    ]
    |}];
  print_reverse_edges ~code:16;
  [%expect
    {|
    [ { target = 1; label = { code = 32; may_be_located = "100000" } }
    ; { target = 1; label = { code = 48; may_be_located = "110000" } }
    ; { target = 2; label = { code = 32; may_be_located = "100000" } }
    ; { target = 2; label = { code = 40; may_be_located = "101000" } }
    ; { target = 3; label = { code = 32; may_be_located = "100000" } }
    ; { target = 3; label = { code = 36; may_be_located = "100100" } }
    ; { target = 4; label = { code = 32; may_be_located = "100000" } }
    ; { target = 4; label = { code = 34; may_be_located = "100010" } }
    ; { target = 5; label = { code = 32; may_be_located = "100000" } }
    ; { target = 5; label = { code = 33; may_be_located = "100001" } }
    ]
    |}];
  print_reverse_edges ~code:32;
  [%expect {| [] |}];
  print_reverse_edges ~code:48;
  [%expect {| [] |}];
  print_reverse_edges ~code:40;
  [%expect
    {|
    [ { target = 0; label = { code = 16; may_be_located = "010000" } }
    ; { target = 0; label = { code = 48; may_be_located = "110000" } }
    ; { target = 2; label = { code = 16; may_be_located = "010000" } }
    ; { target = 2; label = { code = 24; may_be_located = "011000" } }
    ; { target = 3; label = { code = 16; may_be_located = "010000" } }
    ; { target = 3; label = { code = 20; may_be_located = "010100" } }
    ; { target = 4; label = { code = 16; may_be_located = "010000" } }
    ; { target = 4; label = { code = 18; may_be_located = "010010" } }
    ; { target = 5; label = { code = 16; may_be_located = "010000" } }
    ; { target = 5; label = { code = 17; may_be_located = "010001" } }
    ]
    |}];
  ()
;;

let%expect_test "solve size 6" =
  let t = Automaton.create ~size:6 in
  let solutions = Automaton.catch_the_bunny t in
  Dyn.print (solutions |> Dyn.list Automaton.Solution.to_dyn);
  [%expect
    {|
    [ { length = 8; sequence = [ 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 8; sequence = [ 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 1; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 9; sequence = [ 4; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 9; sequence = [ 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 9; sequence = [ 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 9; sequence = [ 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 9; sequence = [ 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 9; sequence = [ 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 9; sequence = [ 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 2; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 1; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 1; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 4; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 3; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 10; sequence = [ 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 10; sequence = [ 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 2; 4; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 1; 4; 3; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 1; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 1; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 1; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 1; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 1; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 4; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 4; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 4; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 4; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 4; 1; 2; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 11; sequence = [ 4; 3; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 1; 2; 4; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 1; 4; 3; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 4; 3; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 4; 3; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 4; 3; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 4; 3; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 1; 4; 3; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 1; 2; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 1; 2; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 1; 2; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 1; 2; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 1; 2; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 1; 2; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 3; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 12; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 1; 2; 4; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 1; 2; 4; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 1; 2; 4; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 1; 2; 4; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 1; 2; 4; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 1; 4; 3; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 4; 3; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 4; 3; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 4; 3; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 4; 3; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 1; 2; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 1; 2; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 1; 2; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 1; 2; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 1; 2; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 3; 1; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 3; 1; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 3; 1; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 3; 1; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 3; 1; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 4; 3; 2; 1 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 13; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 0; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 2; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 3; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 4; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 0; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 1; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 2; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 3; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 5; 1; 2; 3; 4 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 14; sequence = [ 4; 3; 2; 4; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 1; 2; 3; 1; 4; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 1; 4; 3; 2; 4; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 0; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 2; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 3; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 4; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 4; 1; 2; 3; 1; 4; 3; 2; 1; 5; 4; 1; 2; 3; 4 ] }
    ; { length = 15; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 0; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 1; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 2; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 3; 1; 4; 3; 2; 1 ] }
    ; { length = 15; sequence = [ 4; 3; 2; 4; 1; 1; 2; 3; 4; 5; 1; 4; 3; 2; 1 ] }
    ]
    |}];
  ()
;;

let%expect_test "sequence size 6" =
  let t = Automaton.create ~size:6 in
  let print_sequence sequence =
    Dyn.print (Automaton.execute_sequence t ~sequence |> Dyn.list Automaton.Step.to_dyn)
  in
  print_sequence [ 1; 2; 3; 4; 4; 3; 2; 1 ];
  [%expect
    {|
    [ Status_line { code = 63; may_be_located = "111111" }
    ; Open_box 1
    ; Status_line { code = 47; may_be_located = "101111" }
    ; Bunny_moved { code = 31; may_be_located = "011111" }
    ; Open_box 2
    ; Status_line { code = 23; may_be_located = "010111" }
    ; Bunny_moved { code = 47; may_be_located = "101111" }
    ; Open_box 3
    ; Status_line { code = 43; may_be_located = "101011" }
    ; Bunny_moved { code = 23; may_be_located = "010111" }
    ; Open_box 4
    ; Status_line { code = 21; may_be_located = "010101" }
    ; Bunny_moved { code = 42; may_be_located = "101010" }
    ; Open_box 4
    ; Status_line { code = 40; may_be_located = "101000" }
    ; Bunny_moved { code = 20; may_be_located = "010100" }
    ; Open_box 3
    ; Status_line { code = 16; may_be_located = "010000" }
    ; Bunny_moved { code = 40; may_be_located = "101000" }
    ; Open_box 2
    ; Status_line { code = 32; may_be_located = "100000" }
    ; Bunny_moved { code = 16; may_be_located = "010000" }
    ; Open_box 1
    ; Bunny_was_caught
    ]
    |}];
  ()
;;

let%expect_test "edges size 3" =
  let t = Automaton.create ~size:3 in
  let print_edges ~code =
    let edges = Automaton.edges t ~code in
    Dyn.print (edges |> Dyn.list Automaton.Edge.to_dyn)
  in
  print_edges ~code:0;
  [%expect
    {|
    [ { target = 0; label = { code = 0; may_be_located = "000" } }
    ; { target = 1; label = { code = 0; may_be_located = "000" } }
    ; { target = 2; label = { code = 0; may_be_located = "000" } }
    ]
    |}];
  require_does_raise (fun () -> print_edges ~code:(-1));
  [%expect
    {|
    Code out of bounds.
    { code = -1; length = 8 }
    |}];
  require_does_raise (fun () -> print_edges ~code:255);
  [%expect
    {|
    Code out of bounds.
    { code = 255; length = 8 }
    |}];
  ()
;;
