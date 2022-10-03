(* Tests for lab2.ml *)

open OUnit2
open Lab2
open Num

let square n = n * n
let cube n = n * n * n
let inc n = n + 1
let step1 n = n + 1
let step2 n = n + 2

let ni = num_of_int
let squaren n = n */ n
let cuben n = n */ n */ n
let step1n n = n +/ (ni 1)
let step2n n = n +/ (ni 2)
let pi = atan 1.0 *. 4.

let rec range = function 1 -> [1] | n -> range (n-1) @ [n]

let all_tests = "all" >:::
[ 
  "fast_expt" >:: (fun c -> 
     assert_equal (fast_expt 2 0) 1;
     assert_equal (fast_expt 2 5) 32;
     assert_equal (fast_expt 2 16) 65536;
     assert_equal (fast_expt 3 4) 81;
     assert_equal (fast_expt 14 5) 537824
  );

  "ifast_expt" >:: (fun c -> 
     assert_equal (ifast_expt 2 0) 1;
     assert_equal (ifast_expt 2 5) 32;
     assert_equal (ifast_expt 2 16) 65536;
     assert_equal (ifast_expt 3 4) 81;
     assert_equal (ifast_expt 14 5) 537824
  );

  "fast_mult" >:: (fun c ->
     assert_equal (fast_mult 2 0) 0;
     assert_equal (fast_mult 2 1) 2;
     assert_equal (fast_mult 2 7) 14;
     assert_equal (fast_mult 2 16) 32;
     assert_equal (fast_mult 3 4) 12;
     assert_equal (fast_mult 31 43) 1333
  );

  "ifast_mult" >:: (fun c ->
     assert_equal (ifast_mult 2 0) 0;
     assert_equal (ifast_mult 2 1) 2;
     assert_equal (ifast_mult 2 7) 14;
     assert_equal (ifast_mult 2 16) 32;
     assert_equal (ifast_mult 3 4) 12;
     assert_equal (ifast_mult 31 43) 1333
  );

  "isum" >:: (fun c ->
     assert_equal (isum squaren (ni 10) step1n (ni 0)) (ni 0);
     assert_equal (isum squaren (ni 4) step1n (ni 4)) (ni 16);
     assert_equal (isum squaren (ni 0) step1n (ni 10)) (ni 385)
  );

  "product_rec" >:: (fun c ->
     assert_equal (product_rec squaren (ni 10) step1n (ni 1)) (ni 1);
     assert_equal (product_rec squaren (ni 1) step1n (ni 5)) (ni 14400);
     assert_equal (product_rec cuben (ni 1) step2n (ni 5)) (ni 3375)
  );

  "factorial_rec" >:: (fun c ->
     assert_equal (factorial_rec (ni 0)) (ni 1);
     assert_equal (factorial_rec (ni 5)) (ni 120);
     assert_equal (factorial_rec (ni 10)) (ni 3628800);
  );

  "product_iter" >:: (fun c ->
     assert_equal (product_iter squaren (ni 10) step1n (ni 1)) (ni 1);
     assert_equal (product_iter squaren (ni 1) step1n (ni 5)) (ni 14400);
     assert_equal (product_iter cuben (ni 1) step2n (ni 5)) (ni 3375)
  );

  "factorial_iter" >:: (fun c ->
     assert_equal (factorial_iter (ni 0)) (ni 1);
     assert_equal (factorial_iter (ni 5)) (ni 120);
     assert_equal (factorial_iter (ni 10)) (ni 3628800);
  );

  "pi_approx" >:: (fun c ->
     assert_bool "pi approximation out of range" 
       (cmp_float ~epsilon:0.001 pi pi_approx)
  );

(*
  "accumulate_rec" >:: (fun c ->
     assert_equal 0 0  (* TODO *)
  );

  "accumulate_iter" >:: (fun c ->
     assert_equal 0 0  (* TODO *)
  );
*)

  "sum" >:: (fun c ->
     assert_equal (sum squaren (ni 10) step1n (ni 1)) (ni 0);
     assert_equal (sum squaren (ni 1) step1n (ni 5)) (ni 55);
     assert_equal (sum cuben (ni 1) step2n (ni 5)) (ni 153)
  );

  "product" >:: (fun c ->
     assert_equal (product squaren (ni 10) step1n (ni 1)) (ni 1);
     assert_equal (product squaren (ni 1) step1n (ni 5)) (ni 14400);
     assert_equal (product cuben (ni 1) step2n (ni 5)) (ni 3375)
  );

  "compose" >:: (fun c ->
     assert_equal ((compose square step1) 6) 49;
     assert_equal ((compose step1 square) 6) 37;
     assert_equal ((compose cube step2) 6) 512;
     assert_equal ((compose step2 cube) 6) 218
  );

  "repeated" >:: (fun c ->
     assert_equal ((repeated square 2) 5) 625;
     assert_equal ((repeated cube 3) 2) 134217728
  );

(*
  "smooth" >:: (fun c ->
     assert_equal 0 0  (* TODO *)
  );

  "nsmoothed" >:: (fun c ->
     assert_equal 0 0  (* TODO *)
  );
*)

  "is_prime" >:: (fun c ->
     assert_equal (List.filter is_prime (range 100))
       [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 
        59; 61; 67; 71; 73; 79; 83; 89; 97];
     assert_equal (List.map is_prime [-2; -1; 0; 1]) 
       [false; false; false; false]
  );

  "smallest_prime_factor" >:: (fun c ->
     let r = [4; 6; 8; 9; 10; 12; 14; 15; 16; 18; 20; 21; 22; 24; 25; 
              26; 27; 28; 30; 32; 33; 34; 35; 36; 38; 39; 40; 42; 44; 
              45; 46; 48; 49; 50; 51; 52; 54; 55; 56; 57; 58; 60; 62; 
              63; 64; 65; 66; 68; 69; 70; 72; 74; 75; 76; 77; 78; 80; 
              81; 82; 84; 85; 86; 87; 88; 90; 91; 92; 93; 94; 95; 96; 
              98; 99; 100] in
     assert_equal (List.map smallest_prime_factor r) 
       [2; 2; 2; 3; 2; 2; 2; 3; 2; 2; 2; 3; 2; 2; 5; 2; 3; 2; 2; 2; 3; 
        2; 5; 2; 2; 3; 2; 2; 2; 3; 2; 2; 7; 2; 3; 2; 2; 5; 2; 3; 2; 2; 
        2; 3; 2; 5; 2; 2; 3; 2; 2; 2; 3; 2; 7; 2; 2; 3; 2; 2; 5; 2; 3; 
        2; 2; 7; 2; 3; 2; 5; 2; 2; 3; 2];
     assert_equal (smallest_prime_factor 42009217) 641
  )
]

let _ = run_test_tt_main all_tests

