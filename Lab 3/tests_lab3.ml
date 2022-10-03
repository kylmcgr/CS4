(* Tests for lab3.ml *)

open OUnit2
open Lab3

let check_within msg f1 f2 prec =
  assert_bool msg (cmp_float ~epsilon:prec f1 f2)

let square n = n * n

(* Compare two lists for "set equality" i.e. that they have the
 * same elements.  Here we require that no list have extra duplicates
 * of existing elements that the other list doesn't have. *)
let rec set_equal lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

let eps = 1.0e-8

let all_tests = "all" >:::
[ 
(*** Problem A.1 ***)
  "last_sublist" >:: (fun c ->
    assert_raises (Invalid_argument "last_sublist: empty list") 
      (fun () -> last_sublist []);
    assert_equal  (last_sublist [1]) [1];
    assert_equal  (last_sublist [1; 2; 3; 4; 5]) [5]
  );

(*** Problem A.2 ***)

  "reverse" >:: (fun c ->
    assert_equal (reverse []) [];
    assert_equal (reverse [1; 2; 3; 4; 5]) [5; 4; 3; 2; 1];
    assert_equal (reverse [[1; 4]; [9]; [16; 25]]) [[16; 25]; [9]; [1; 4]]
  );

(*** Problem A.3 ***)

  "square_list" >:: (fun c ->
    assert_equal (square_list []) [];
    assert_equal (square_list [2]) [4];
    assert_equal (square_list [-2]) [4];
    assert_equal (square_list [-2; 2; -2]) [4; 4; 4];
    assert_equal (square_list [-3; 2; -1; 0]) [9; 4; 1; 0];
    assert_equal (square_list [1; 2; 3; 4]) [1; 4; 9; 16];
    assert_equal (square_list [1; 2; 3; 4; 5]) [1; 4; 9; 16; 25];
  );

  "square_list2" >:: (fun c ->
    assert_equal (square_list2 []) [];
    assert_equal (square_list2 [2]) [4];
    assert_equal (square_list2 [-2]) [4];
    assert_equal (square_list2 [-2; 2; -2]) [4; 4; 4];
    assert_equal (square_list2 [-3; 2; -1; 0]) [9; 4; 1; 0];
    assert_equal (square_list2 [1; 2; 3; 4]) [1; 4; 9; 16];
    assert_equal (square_list2 [1; 2; 3; 4; 5]) [1; 4; 9; 16; 25];
  );

(*** Problem A.4: no tests ***)

(*** Problem A.5 ***)

  "count_negative_numbers" >:: (fun c ->
    assert_equal (count_negative_numbers []) 0;
    assert_equal (count_negative_numbers [-2; -1; 0; 1; 2]) 2;
    assert_equal (count_negative_numbers [1; 2; 3; 4; 5]) 0;
    assert_equal (count_negative_numbers [-2; 0; -2; 0; -1; 1]) 3
  );

(*** Problem A.6 ***)

  "power_of_two_list" >:: (fun c ->
    assert_equal (power_of_two_list 0) [];
    assert_equal (power_of_two_list 1) [1];
    assert_equal (power_of_two_list 5) [1; 2; 4; 8; 16]
  );

(*** Problem A.7 ***)

  "prefix_sum" >:: (fun c ->
    assert_equal (prefix_sum []) [];
    assert_equal (prefix_sum [1]) [1];
    assert_equal (prefix_sum [1; 1; 1; 1]) [1; 2; 3; 4];
    assert_equal (prefix_sum [1; -1; 1; -1]) [1; 0; 1; 0];
    assert_equal (prefix_sum [1; 2; 3; 4]) [1; 3; 6; 10]
  );

(*** Problem A.8 ***)

  "deep_reverse" >:: (fun c ->
    assert_equal (deep_reverse []) [];
    assert_equal (deep_reverse [[1; 1]; [2; 2]]) [[2; 2]; [1; 1]];
    assert_equal (deep_reverse [[1; 2]; [3; 4]]) [[4; 3]; [2; 1]];
    assert_equal (deep_reverse [[[1; 2]; [3; 4]]; [[5; 6]; [7; 8]]])
     [[[7; 8]; [5; 6]]; [[3; 4]; [1; 2]]]
   );

(*** Problem A.9 ***)

  "deep_reverse_nested" >:: (fun c ->
    assert_equal (deep_reverse_nested (Value 10)) (Value 10);
    assert_equal (deep_reverse_nested 
      (List [Value 10; Value 20; Value 30; Value 40])) 
      (List [Value 40; Value 30; Value 20; Value 10]);
    assert_equal (deep_reverse_nested 
      (List [List [Value 10; Value 20]; List [Value 30; Value 40]])) 
      (List [List [Value 40; Value 30]; List [Value 20; Value 10]]);
    assert_equal (deep_reverse_nested 
      (List [Value 10; List [Value 20; Value 30]])) 
      (List [List [Value 30; Value 20]; Value 10]);
    assert_equal (deep_reverse_nested 
      (List [List [Value 10; Value 20]; Value 30]))
      (List [Value 30; List [Value 20; Value 10]]);
    assert_equal (deep_reverse_nested 
      (List [Value 10; List [Value 20; List [Value 30; Value 40]; Value 50]; Value 60])) 
      (List [Value 60; List [Value 50; List [Value 40; Value 30]; Value 20]; Value 10])
  );

(*** Problem B.1 ***)

  "quicksort" >:: (fun c ->
     assert_equal (quicksort [] (<)) [];
     assert_equal (quicksort [1] (<)) [1];
     assert_equal (quicksort [1;2;3;4;5] (<)) [1;2;3;4;5];
     assert_equal (quicksort [5;4;3;2;1;1;2;3;4;5] (<)) [1;1;2;2;3;3;4;4;5;5];
     assert_equal (quicksort [5;4;3;2;1;1;2;3;4;5] (>)) [5;5;4;4;3;3;2;2;1;1]
  );

(*** Problem B.4 ***)

  "insertion_sort" >:: (fun c ->
     assert_equal (insertion_sort [] (<)) [];
     assert_equal (insertion_sort [1] (<)) [1];
     assert_equal (insertion_sort [1;2;3;4;5] (<)) [1;2;3;4;5];
     assert_equal (insertion_sort [5;4;3;2;1;1;2;3;4;5] (<)) [1;1;2;2;3;3;4;4;5;5];
     assert_equal (insertion_sort [5;4;3;2;1;1;2;3;4;5] (>)) [5;5;4;4;3;3;2;2;1;1]
  );

(*** Problem C.1 ***)

  "subsets" >:: (fun c ->
     assert_equal (subsets []) [[]];
     assert_bool "C.1.2" (set_equal (subsets [1]) [[];[1]]);
     assert_bool "C.1.3" (set_equal
       (subsets [1;2;3]) 
       [[];[3];[2];[2;3];[1];[1;3];[1;2];[1;2;3]]);
  );

(*** Problem C.2 ***)

  "map, append, length" >:: (fun c ->
     assert_equal (map square [1;2;3;4;5]) [1;4;9;16;25];
     assert_equal (map square []) [];
     assert_equal (append [] []) [];
     assert_equal (append [1] [2]) [1;2];
     assert_equal (append [1;2;3;4;5] []) [1;2;3;4;5];
     assert_equal (append [] [1;2;3;4;5]) [1;2;3;4;5];
     assert_equal (append [1;2;3;4;5] [6;7;8;9;10]) [1;2;3;4;5;6;7;8;9;10];
     assert_equal (length []) 0;
     assert_equal (length [1;2;3;4;5]) 5
  );

  "accumulate_n" >:: (fun c ->
     assert_equal (accumulate_n (+) 0 [[];[];[]]) [];
     assert_equal (accumulate_n (+) 0 [[1;2;3];[4;5;6];[7;8;9];[10;11;12]])
       [22;26;30];
     assert_equal (accumulate_n ( * ) 1 [[2;3];[4;5]]) [8;15]
  );

  "dot_product, matrix_times_vector, matrix_times_matrix" >:: (fun c ->
     assert_equal (dot_product [] []) 0;
     assert_equal (dot_product [1;2;3] [4;5;6]) 32;
     assert_equal (matrix_times_vector [[1;0];[0;1]] [10;20]) [10;20];
     assert_equal (matrix_times_vector [[1;2];[3;4]] [-2;3]) [4;6];
     assert_equal (transpose [[1;2];[3;4]]) [[1;3];[2;4]];
     assert_equal (transpose [[1;2;3];[4;5;6]]) [[1;4];[2;5];[3;6]];
     assert_equal (matrix_times_matrix [[1;0];[0;1]] [[1;2];[3;4]]) 
                                       [[1;2];[3;4]];
     assert_equal (matrix_times_matrix [[1;2];[3;4]] [[1;2];[3;4]]) 
                                       [[7;10];[15;22]];
     assert_equal (matrix_times_matrix [[1;2;3];[4;5;6]] [[1;2];[3;4];[5;6]])
                                       [[22;28];[49;64]]
  );

]

let _ = run_test_tt_main all_tests

