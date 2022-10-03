(* Tests for lab5.ml *)

open OUnit2
open Lab5

let eps = 0.00000001
let assert_float msg f1 f2 = assert_bool msg (cmp_float ~epsilon:eps f1 f2)
let ungram (`Gram f) = f
let unsecond (`Second s) = s

(* Test function for bubble sort. *)
let test_bs arr = bubble_sort arr; arr

let all_tests = "all" >:::
[ 
  "Problem A.1" >:: (fun c ->
     assert_equal (fibonacci 0) 0;
     assert_equal (fibonacci 1) 1;
     assert_equal (fibonacci 2) 1;
     assert_equal (fibonacci 3) 2;
     assert_equal (fibonacci 5) 5;
     assert_equal (fibonacci 10) 55;
     assert_equal (fibonacci 40) 102334155;
     assert_equal (fibonacci2 0) 0;
     assert_equal (fibonacci2 1) 1;
     assert_equal (fibonacci2 2) 1;
     assert_equal (fibonacci2 3) 2;
     assert_equal (fibonacci2 5) 5;
     assert_equal (fibonacci2 10) 55;
     assert_equal (fibonacci2 40) 102334155
  );

  "Problem A.2" >:: (fun c ->
     assert_equal (test_bs [| |]) [| |];
     assert_equal (test_bs [| 1 |]) [| 1 |];
     assert_equal (test_bs [| 1; 1 |]) [| 1; 1 |];
     assert_equal (test_bs [| 2; 1 |]) [| 1; 2 |];
     assert_equal (test_bs [| 1; 1; 1 |]) [| 1 ; 1; 1 |];
     assert_equal (test_bs [| 1; 2; 3; 4; 5 |]) [| 1 ; 2; 3; 4; 5 |];
     assert_equal (test_bs [| 5; 2; 3; 1; 4 |]) [| 1 ; 2; 3; 4; 5 |];
     assert_equal (test_bs [| 5; 3; 3; 5; 1 |]) [| 1 ; 3; 3; 5; 5 |];
     assert_equal (test_bs [| "foo"; "bar"; "baz" |])
      [| "bar"; "baz"; "foo" |]
  );

  "Problem B.1" >:: (fun c ->
     assert_float "get_meters 1"   (get_meters   (`Meter  1.0)) 1.0;
     assert_float "get_meters 2"   (get_meters   (`Foot   2.0)) 0.6096;
     assert_float "get_meters 3"   (get_meters   (`Inch   3.0)) 0.0762;
     assert_float "get_grams 1"    (get_grams    (`Gram   1.0)) 1.0;
     assert_float "get_grams 2"    (get_grams    (`Kilo   2.0)) 2000.0;
     assert_float "get_grams 3"    (get_grams    (`Slug   3.0)) 43781.709609;
     assert_float "get_seconds 1"  (get_seconds  (`Second 1.0)) 1.0;
     assert_float "get_seconds 2"  (get_seconds  (`Minute 2.0)) 120.0;
     assert_float "get_seconds 3"  (get_seconds  (`Hour   3.0)) 10800.0;
     assert_float "get_seconds 4"  (get_seconds  (`Day    4.0)) 345600.0;
     assert_float "mass_add 1" (ungram (mass_add (`Gram 1.0) (`Slug 2.0)))
       29188.806406;
     assert_float "mass_add 2" (ungram (mass_add (`Gram 1.0) (`Kilo 2.0)))
       2001.0;
     assert_float "mass_add 3" (ungram (mass_add (`Slug 1.0) (`Kilo 2.0)))
      16593.903203;
     assert_float "time_add 1" (unsecond (time_add (`Second 1.0) (`Minute 2.0)))
       121.0;
     assert_float "time_add 2" (unsecond (time_add (`Minute 1.0) (`Hour 2.0)))
       7260.0;
     assert_float "time_add 3" (unsecond (time_add (`Hour 1.0) (`Day 2.0)))
       176400.0;
     assert_float "time_add 4" (unsecond (time_add (`Day 1.0) (`Second 2.0)))
       86402.0
  );
]

let _ = run_test_tt_main all_tests

