(* Tests for lab 7. *)

(*
To run this test script from the ocaml toplevel, do this:

$ ocaml
#use "topfind";;  (* may not be necessary *)
#require "oUnit";;
#load "boards.cmo";;
#load "klotski.cmo";;
#use "tests_lab7.ml";;
# run_tests ()  (* assuming this line is commented out below *)
*)

open OUnit2
open Boards
open Klotski

(*** Generated modules. ***)

(* Module for a set of strings. *)
module StringSet =
  Set.Make(struct
             type t = string
             let compare = Stdlib.compare
           end)

(*** Some extra boards for testing. ***)

let empty_board =
  "...." ^
  "...." ^
  "...." ^
  "...." ^
  "...."

let test_board1 =
  ".aa." ^
  "...." ^
  "...." ^
  "...." ^
  "...."

let test_board2 =
  ".aa." ^
  "...." ^
  "bb.." ^
  "...." ^
  "...."

let test_board3 =
  ".aa." ^
  "...." ^
  "bb.." ^
  ".cc." ^
  ".cc."

let test_board4 =
  "...." ^
  "...." ^
  "...." ^
  ".aa." ^
  ".aa."

let test_board5 =
  "abbc" ^
  "addc" ^
  "eddf" ^
  "gghi" ^
  ".jh."

let test_board6 =
  "jiih" ^
  "jggh" ^
  "fgge" ^
  "ddcb" ^
  ".ac."

let test_board7 =
  "aabb" ^
  "aabb" ^
  "ccdd" ^
  "effg" ^
  "effg"

let test_board8 =
  "aabb" ^
  "aabb" ^
  "ccdd" ^
  "eff." ^
  "eff."

let test_board9 =
  "...." ^
  "aa.." ^
  "aa.." ^
  "...." ^
  "...."

let test_board9b =
  "...." ^
  "bc.." ^
  "bc.." ^
  "...." ^
  "...."

let test_board10 =
  "...." ^
  "...." ^
  ".g.." ^
  "...." ^
  "...."

let test_board11 =
  "ac.." ^
  "b..." ^
  "...." ^
  "...." ^
  "...."

let test_board11b =
  "aa.." ^
  "b..." ^
  "...." ^
  "...." ^
  "...."

(*** Boards with pieces removed. ***)

let board0_minus_a =
  "..bb.cc.ddeeffghffij"

let board0_minus_b =
  "aa...cc.ddeeffghffij"

let board0_minus_c =
  "aabb....ddeeffghffij"

let board0_minus_d =
  "aabb.cc...eeffghffij"

let board0_minus_e =
  "aabb.cc.dd..ffghffij"

let board0_minus_f =
  "aabb.cc.ddee..gh..ij"

let board0_minus_g =
  "aabb.cc.ddeeff.hffij"

let board0_minus_h =
  "aabb.cc.ddeeffg.ffij"

let board0_minus_i =
  "aabb.cc.ddeeffghff.j"

let board0_minus_j =
  "aabb.cc.ddeeffghffi."

(*** Locations of pieces on a board. ***)

let board0_a = [(0, 0); (0, 1)]
let board0_b = [(0, 2); (0, 3)]
let board0_c = [(1, 1); (1, 2)]
let board0_d = [(2, 0); (2, 1)]
let board0_e = [(2, 2); (2, 3)]
let board0_f = [(3, 0); (3, 1); (4, 0); (4, 1)]
let board0_g = [(3, 2)]
let board0_h = [(3, 3)]
let board0_i = [(4, 2)]
let board0_j = [(4, 3)]

(*** Board lists in string form. ***)

let test_next_board0 =
  ["aabbcc..ddeeffghffij";
   "aabb..ccddeeffghffij" ]

let test_next_board1 =
  ["aa..bbccdeffgghigghi";
   "..aabbccdeffgghigghi"]

let test_next_board2 =
  ["aabcaabc.efgdhhgd...";
   "aabcaabcdef.dhhg...g";
   "aabcaabcdefgd..g.hh."]

let test_next_board3 =
  ["a...abbc.bbcdeefdggf";
   ".bb.abbca..cdeefdggf";
   "...cabbcabb.deefdggf"]

let test_next_board4 =
  ["abbcdefgdhhg.hhjikk.";
   "abbcdefgdhhgihh..kkj";
   "abbcdefgdhhgihhjkk..";
   "abbcdefgdhhgihhj..kk"]

let test_next_board5 =
  ["abbcabbcdefgh..jhiij"]

let test_next_board6 =
  ["abbcabbcdeefd.hfig.j";
   "abbcabbcdeefdg.fi.hj";
   "abbcabbcdeefdghf..ij";
   "abbcabbcdeefdghf.i.j";
   "abbcabbcdeefdghfij..";
   "abbcabbcdeefdghfi.j."]

let test_next_board7 =
  ["abcdeefdeef.hiigjj.g";
   "abcdeefdeefghiig..jj";
   "abcdeefdeefghiig.jj."]

let test_next_empty_board = []

let test_next_test_board1 =
  [".................aa.";
   ".............aa.....";
   ".........aa.........";
   ".....aa.............";
   "aa..................";
   "..aa................"]

let test_next_test_board7 = []

let test_next_test_board9 =
  ["aa..aa..............";
   "............aa..aa..";
   "........aa..aa......";
   "......aa..aa........";
   ".....aa..aa........."]

let test_next_test_board10 =
  [".g..................";
   ".....g..............";
   ".................g..";
   ".............g......";
   "........g...........";
   "...........g........";
   "..........g........."]

(*** Move lists in string form, together with the (string) boards
 *** those moves generate. ***)

let moves_board0 =
  [(('c', Left, 1), "aabbcc..ddeeffghffij");
   (('c', Right, 1), "aabb..ccddeeffghffij")]

let moves_board1 =
  [(('a', Left, 1), "aa..bbccdeffgghigghi");
   (('a', Right, 1), "..aabbccdeffgghigghi")]

let moves_board2 =
  [(('d', Down, 1), "aabcaabc.efgdhhgd...");
   (('g', Down, 1), "aabcaabcdef.dhhg...g");
   (('h', Down, 1), "aabcaabcdefgd..g.hh.")]

let moves_board3 =
  [(('a', Up, 1), "a...abbc.bbcdeefdggf");
   (('b', Up, 1), ".bb.abbca..cdeefdggf");
   (('c', Up, 1), "...cabbcabb.deefdggf")]

let moves_board4 =
  [(('i', Down, 1), "abbcdefgdhhg.hhjikk.");
   (('j', Down, 1), "abbcdefgdhhgihh..kkj");
   (('k', Left, 1), "abbcdefgdhhgihhjkk..");
   (('k', Right, 1), "abbcdefgdhhgihhj..kk")]

let moves_board5 =
  [(('i', Down, 1), "abbcabbcdefgh..jhiij")]

let moves_board6 =
  [(('g', Down, 1), "abbcabbcdeefd.hfig.j");
   (('h', Down, 1), "abbcabbcdeefdg.fi.hj");
   (('i', Right, 1), "abbcabbcdeefdghf.i.j");
   (('i', Right, 2), "abbcabbcdeefdghf..ij");
   (('j', Left, 1), "abbcabbcdeefdghfi.j.");
   (('j', Left, 2), "abbcabbcdeefdghfij..")]

let moves_board7 =
  [(('g', Down, 1), "abcdeefdeef.hiigjj.g");
   (('j', Right, 1), "abcdeefdeefghiig.jj.");
   (('j', Right, 2), "abcdeefdeefghiig..jj")]

let moves_test_board9 =
  [(('a', Up, 1), "aa..aa..............");
   (('a', Right, 1), ".....aa..aa.........");
   (('a', Right, 2), "......aa..aa........");
   (('a', Down, 1), "........aa..aa......");
   (('a', Down, 2), "............aa..aa..")]

let bad_moves_board0 =
  [(':', Left, 1);
   ('z', Left, 1);
   ('k', Left, 1);
   ('c', Left, 2);
   ('c', Right, 2);
   ('c', Up, 1);
   ('c', Down, 1)]

let bad_moves_test_board10 =
  [('a', Left, 1);
   ('g', Left, 2);
   ('g', Right, 3);
   ('g', Up, 3);
   ('g', Down, 3);
   ('g', Right, 0);
   ('g', Right, -1)]

let bad_moves_test_board11 =
  [('a', Right, 2);
   ('a', Right, 3);
   ('a', Down, 2);
   ('a', Down, 3);
   ('a', Down, 4)]

(*** Utility functions. ***)

(* Use a specific random number seed if you need to reproduce results.
 * For instance, if a randomized test fails you might want to use this. *)
(*
let _ = Random.init 12345
*)

(* Use an arbitrary random number seed. *)
let _ = Random.self_init ()

(* Strip newlines from a string. *)
let strip s = Str.global_replace (Str.regexp "\n") "" s

let expect_solved i b result =
  let name = Printf.sprintf "is_solved test %d" i in
    assert_bool name (is_solved (read b) = result)

let expect_equal i b1 b2 =
  let name = Printf.sprintf "compare test %d" i in
    assert_bool name (compare (read b1) (read b2) = 0)

let expect_unequal i b1 b2 =
  let name = Printf.sprintf "compare test %d" i in
    assert_bool name (compare (read b1) (read b2) <> 0)

(* FIXME
 * Just comparing unoccupied squares passes all the tests.
 * There should be two boards with the same unoccupied squares
 * but different pieces.
 *)

(* Unequal boards should return -1 or 1, and flipping the
 * order of arguments should flip the sign. *)
let expect_opposite i b1 b2 =
  let name = Printf.sprintf "compare test %d" i in
  let b1'  = read b1 in
  let b2'  = read b2 in
  let c1   = compare b1' b2' in
  let c2   = compare b2' b1' in
    assert_bool name (abs c1 = 1 && abs c2 = 1 && c1 = -c2)

let expect_remove i b1 c b2 =
  let name = Printf.sprintf "remove test %d" i in
  let b1'  = read b1 in
  let b2'  = remove c b1' in
  let b2s  = strip (show b2') in
    assert_bool name (b2s = b2)

(* Removing a piece and adding it back should yield the same board. *)
let expect_remove_add i b c =
  let name = Printf.sprintf "remove/add test %d" i in
  try
    let b'   = read b in
    let p    = CharMap.find c b'.pieces in
    let b2   = remove c b' in
      begin
        match add (c, p) b2 with
          | None -> assert_bool name false
          | Some b3 ->
              let b's  = strip (show b') in
              let b3s  = strip (show b3) in
                assert_bool name (b's = b3s)
      end
  with
    Not_found ->  (* should never happen *)
      assert_bool name false

let expect_add i b1 (c, p) b2 =
  let name = Printf.sprintf "add test %d" i in
  let b1'  = read b1 in
  let p'   = LocSet.of_list p in
    match add (c, p') b1' with
      | None -> assert_bool name false
      | Some b2' ->
          let b2s  = strip (show b2') in
            assert_bool name (b2s = b2)

let expect_valid_move_board i b bms =
  let test_move m b expected =
    match make_move m b with
      | None -> false
      | Some b' -> expected = strip (show b')
  in
  let name = Printf.sprintf "make_move test %d" i in
  let b'   = read b in
  List.iter
    (fun (m, nb) -> assert_bool name (test_move m b' nb))
    bms

let expect_invalid_move i b ms =
  let name = Printf.sprintf "make_move test %d" i in
  let b'   = read b in
  List.iter
    (fun m -> assert_bool name (make_move m b' = None))
    ms

(* Given a board string, check that the boards that can be
 * derived from the original board by making a single move
 * are the expected ones. *)
let expect_nexts i b nexts =
  let get_nexts b =
    let b' = read b in
    let ns = next b' in
      StringSet.of_list (List.map (fun n -> strip (show n)) ns)
  in
  let name = Printf.sprintf "next test %d" i in
  let nexts' = StringSet.of_list nexts in
  let c = StringSet.compare nexts' (get_nexts b) in
    assert_bool name (c = 0)

(*** The tests. ***)

let is_solved_tests =
  "is_solved tests" >:: (fun c ->
    expect_solved  0 boards.(0) false;
    expect_solved  1 boards.(1) false;
    expect_solved  2 boards.(2) false;
    expect_solved  3 boards.(3) false;
    expect_solved  4 boards.(4) false;
    expect_solved  5 boards.(5) false;
    expect_solved  6 boards.(6) false;
    expect_solved  7 boards.(7) false;
    expect_solved  8 test_board1 false;
    expect_solved  9 test_board2 false;
    expect_solved 10 test_board3 true;
    expect_solved 11 test_board4 true;
    expect_solved 12 test_board5 false;
    expect_solved 13 test_board6 false;
    expect_solved 14 test_board7 true;
    expect_solved 15 empty_board false;
  )

let compare_tests =
  "compare tests" >:: (fun c ->
     expect_equal 0 boards.(0) boards.(0);
     expect_equal 1 boards.(1) boards.(1);
     expect_equal 2 boards.(2) boards.(2);
     expect_equal 3 boards.(3) boards.(3);
     expect_equal 4 boards.(4) boards.(4);
     expect_equal 5 boards.(5) boards.(5);
     expect_equal 6 boards.(6) boards.(6);
     expect_equal 7 boards.(7) boards.(7);

     expect_unequal  8 boards.(0) boards.(2);
     expect_unequal  9 boards.(1) boards.(7);
     expect_unequal 10 boards.(2) boards.(3);
     expect_unequal 11 boards.(3) boards.(2);
     expect_unequal 12 boards.(4) boards.(0);

     (* Test equivalent boards with different labels. *)
     expect_equal 13 test_board5 test_board6;
     expect_equal 14 test_board6 test_board5;
     expect_equal 15 empty_board empty_board;

     (* Test different boards with the same set of
        unoccupied locations. *)
     expect_unequal 16 test_board9 test_board9b;
     expect_unequal 17 test_board11 test_board11b;

     (* Check that compare returns -1 or 1 for unequal boards.
      * Flipping the order of the boards should flip the signs. *)
     expect_opposite 18 test_board7 test_board8;
     expect_opposite 19 boards.(0) boards.(1);
     expect_opposite 20 boards.(0) empty_board;
  )

let remove_tests =
  "remove tests" >:: (fun c ->
     expect_remove 0 boards.(0) 'a' board0_minus_a;
     expect_remove 1 boards.(0) 'b' board0_minus_b;
     expect_remove 2 boards.(0) 'c' board0_minus_c;
     expect_remove 3 boards.(0) 'd' board0_minus_d;
     expect_remove 4 boards.(0) 'e' board0_minus_e;
     expect_remove 5 boards.(0) 'f' board0_minus_f;
     expect_remove 6 boards.(0) 'g' board0_minus_g;
     expect_remove 7 boards.(0) 'h' board0_minus_h;
     expect_remove 8 boards.(0) 'i' board0_minus_i;
     expect_remove 9 boards.(0) 'j' board0_minus_j;
  )

let add_tests =
  "add tests" >:: (fun c ->
     expect_add 0 board0_minus_a ('a', board0_a) boards.(0);
     expect_add 1 board0_minus_b ('b', board0_b) boards.(0);
     expect_add 2 board0_minus_c ('c', board0_c) boards.(0);
     expect_add 3 board0_minus_d ('d', board0_d) boards.(0);
     expect_add 4 board0_minus_e ('e', board0_e) boards.(0);
     expect_add 5 board0_minus_f ('f', board0_f) boards.(0);
     expect_add 6 board0_minus_g ('g', board0_g) boards.(0);
     expect_add 7 board0_minus_h ('h', board0_h) boards.(0);
     expect_add 8 board0_minus_i ('i', board0_i) boards.(0);
     expect_add 9 board0_minus_j ('j', board0_j) boards.(0);
  )

let remove_add_tests =
  "remove/add tests" >:: (fun c ->
     expect_remove_add 0 boards.(0) 'a';
     expect_remove_add 1 boards.(0) 'b';
     expect_remove_add 2 boards.(0) 'c';
     expect_remove_add 3 boards.(0) 'd';
     expect_remove_add 4 boards.(0) 'e';
     expect_remove_add 5 boards.(0) 'f';
     expect_remove_add 6 boards.(0) 'g';
     expect_remove_add 7 boards.(0) 'h';
     expect_remove_add 8 boards.(0) 'i';
     expect_remove_add 9 boards.(0) 'j';
  )

let make_move_tests =
  "make_move tests" >:: (fun c ->
     expect_valid_move_board 0 boards.(0) moves_board0;
     expect_valid_move_board 1 boards.(1) moves_board1;
     expect_valid_move_board 2 boards.(2) moves_board2;
     expect_valid_move_board 3 boards.(3) moves_board3;
     expect_valid_move_board 4 boards.(4) moves_board4;
     expect_valid_move_board 5 boards.(5) moves_board5;
     expect_valid_move_board 6 boards.(6) moves_board6;
     expect_valid_move_board 7 boards.(7) moves_board7;
     expect_valid_move_board 8 test_board9 moves_test_board9;

     expect_invalid_move  9 boards.(0) bad_moves_board0;
     expect_invalid_move 10 test_board10 bad_moves_test_board10;
     expect_invalid_move 11 test_board11 bad_moves_test_board11;
  )

let next_tests =
  "next tests" >:: (fun c ->
     expect_nexts  0 boards.(0) test_next_board0;
     expect_nexts  1 boards.(1) test_next_board1;
     expect_nexts  2 boards.(2) test_next_board2;
     expect_nexts  3 boards.(3) test_next_board3;
     expect_nexts  4 boards.(4) test_next_board4;
     expect_nexts  5 boards.(5) test_next_board5;
     expect_nexts  6 boards.(6) test_next_board6;
     expect_nexts  7 boards.(7) test_next_board7;
     expect_nexts  8 empty_board  test_next_empty_board;
     expect_nexts  9 test_board1  test_next_test_board1;
     expect_nexts 10 test_board7  test_next_test_board7;
     expect_nexts 11 test_board9  test_next_test_board9;
     expect_nexts 12 test_board10 test_next_test_board10;
  )

let all_tests = "all_tests" >:::
[
  is_solved_tests;
  compare_tests;
  remove_tests;
  add_tests;
  remove_add_tests;
  make_move_tests;
  next_tests
]

let run_tests () =
  begin
    Printf.printf "\nRUNNING KLOTSKI BOARD TESTS...\n\n";
    run_test_tt_main all_tests;
  end

(* Comment this out if you are running this interactively (see above). *)
let _ = run_tests ()

