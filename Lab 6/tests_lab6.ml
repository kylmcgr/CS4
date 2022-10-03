(* Tests for lab6.ml *)

open OUnit2
open Lab6

let eps = 0.00000001
let assert_float msg f1 f2 = assert_bool msg (cmp_float ~epsilon:eps f1 f2)
let s1 = make_stat_1 ()
let s2 = make_stat_2 ()

let all_tests = "all" >:::
[ 
  "Problem B.1" >:: (fun c ->
     assert_raises (Stat_error "need at least one value for mean") 
       (fun () -> s1#mean);
     assert_raises (Stat_error "need at least one value for variance") 
       (fun () -> s1#variance);
     assert_raises (Stat_error "need at least one value for stdev") 
       (fun () -> s1#stdev);
     s1#append 1.0;
     s1#append 2.0;
     s1#append 3.0;
     assert_float "my_stat_1 mean 1" (s1#mean) 2.0;
     s1#append 4.0;
     assert_float "my_stat_1 variance 1" (s1#variance) 1.25;
     assert_float "my_stat_1 stdev 1" (s1#stdev) 1.1180339887498949;
     s1#clear;
     assert_raises (Stat_error "need at least one value for mean") 
       (fun () -> s1#mean);
     assert_raises (Stat_error "need at least one value for variance") 
       (fun () -> s1#variance);
     assert_raises (Stat_error "need at least one value for stdev") 
       (fun () -> s1#stdev);
     s1#append 1.0;
     assert_float "my_stat_1 mean 2" (s1#mean) 1.0;
     assert_float "my_stat_1 variance 2" (s1#variance) 0.0;
     assert_float "my_stat_1 stdev 2" (s1#stdev) 0.0;
  );

  "Problem B.2" >:: (fun c ->
     assert_raises (Stat_error "need at least one value for mean") 
       (fun () -> s2#mean);
     assert_raises (Stat_error "need at least one value for variance") 
       (fun () -> s2#variance);
     assert_raises (Stat_error "need at least one value for stdev") 
       (fun () -> s2#stdev);
     s2#append 1.0;
     s2#append 2.0;
     s2#append 3.0;
     assert_float "my_stat_1 mean 1" (s2#mean) 2.0;
     s2#append 4.0;
     assert_float "my_stat_1 variance 1" (s2#variance) 1.25;
     assert_float "my_stat_1 stdev 1" (s2#stdev) 1.1180339887498949;
     s2#clear;
     assert_raises (Stat_error "need at least one value for mean") 
       (fun () -> s2#mean);
     assert_raises (Stat_error "need at least one value for variance") 
       (fun () -> s2#variance);
     assert_raises (Stat_error "need at least one value for stdev") 
       (fun () -> s2#stdev);
     s2#append 1.0;
     assert_float "my_stat_1 mean 2" (s2#mean) 1.0;
     assert_float "my_stat_1 variance 2" (s2#variance) 0.0;
     assert_float "my_stat_1 stdev 2" (s2#stdev) 0.0;
  );

  "Problem C.1" >:: (fun c ->
     assert_equal (heap_sort []) [];
     assert_equal (heap_sort [5;1;2;4;3]) [1;2;3;4;5];
     assert_equal (heap_sort [5;4;3;2;1;2;3;4;5]) [1;2;2;3;3;4;4;5;5]
  );

  "Problem C.2" >:: (fun c ->
     assert_equal (heap_sort_2 []) [];
     assert_equal (heap_sort_2 ["foo";"bar";"baz"]) ["bar";"baz";"foo"];
     assert_equal (heap_sort_2 ["f";"f";"b"]) ["b";"f";"f"];
     assert_equal (heap_sort_2 ["e";"d";"c";"b";"a";"b";"c";"d";"e"])
       ["a";"b";"b";"c";"c";"d";"d";"e";"e"]
  );

]

let _ = run_test_tt_main all_tests

