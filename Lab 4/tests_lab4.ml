(* Tests for lab4.ml *)

open OUnit2
open Lab4

let check_within msg f1 f2 prec =
  assert_bool msg (cmp_float ~epsilon:prec f1 f2)

let check_within_points msg p1 p2 prec =
  let (x1, y1) = get_coords p1 in
  let (x2, y2) = get_coords p2 in
    check_within msg x1 x2 prec;
    check_within msg y1 y2 prec

let eps = 1.0e-8

let p1 = make_point 0.0 0.0
let p2 = make_point 10.0 0.0
let p3 = make_point 10.0 10.0
let s1 = make_segment p1 p2
let s2 = make_segment p2 p3
let s3 = make_segment p3 p1

let square n = n * n

(* Compare two lists for "set equality" i.e. that they have the
 * same elements.  Here we require that no list have extra duplicates
 * of existing elements that the other list doesn't have. *)
let rec set_equal lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

(* Test cases for mobile problem. *)

let m0 = 
  make_mobile 
    (make_weight 1 1) 
    (make_weight 1 1)

let m1 = 
  make_mobile
    (make_weight 3 4)
    (make_structure 
      4
      (make_mobile
        (make_weight 1 2)
        (make_weight 2 1)))

let m2 =
  make_mobile
    (make_weight 1 400)
    (make_structure 
      10
      (make_mobile
        (make_weight 100 1)
        (make_weight 1 200)))
  
let m3 =
  make_mobile
    (make_weight 1 (total_weight1 m2))
    (make_structure 1 m2)

let m0' = 
  make_mobile'
    (make_weight' 1 1) 
    (make_weight' 1 1)

let m1' = 
  make_mobile'
    (make_weight' 3 4)
    (make_structure' 
      4
      (make_mobile'
        (make_weight' 1 2)
        (make_weight' 2 1)))

let m2' =
  make_mobile'
    (make_weight' 1 400)
    (make_structure' 
      10
      (make_mobile'
        (make_weight' 100 1)
        (make_weight' 1 200)))
  
let m3' =
  make_mobile'
    (make_weight' 1 (total_weight' m2'))
    (make_structure' 1 m2')

(* Test cases for tree problems *)

let tree1 = Tree 
  [Num 10; 
   Sub (Tree [Num 20; 
              Sub (Tree [Num 42; Sub (Tree []); Num 12]); 
              Sub (Tree []);
              Sub (Tree [Num 13; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 2; Num 3])]
  
let tree2 = Tree
  [Num 100; 
   Sub (Tree [Num 400; 
              Sub (Tree [Num 1764; Sub (Tree []); Num 144]); 
              Sub (Tree []);
              Sub (Tree [Num 169; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 4; Num 9])]

let square_tree'' t = tree_map square t 

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d

let all_tests = "all" >:::
[ 
  (*** Part A. ***)

  (* A.1 *)

  "make_point/get_coords" >:: (fun c -> 
     let p = make_point 1.0 3.4 in
     let (x, y) = get_coords p in
       begin
         check_within "invalid x accessor for points" x 1.0 eps;
         check_within "invalid y accessor for points" y 3.4 eps
       end
  );

  "make_segment/get_points" >:: (fun c -> 
     let p1 = make_point 1.0 3.4 in
     let p2 = make_point (-1.0) 43.0 in
     let s = make_segment p1 p2 in
     let (p1', p2') = get_points s in
       begin
         check_within_points "invalid startp accessor for segments" p1' p1 eps;
         check_within_points "invalid endp accessor for segments" p2' p2 eps
       end
  );

  "segment_length" >:: (fun c -> 
     check_within "segment_length s1" (segment_length s1) 10.0 eps;
     check_within "segment_length s2" (segment_length s2) 10.0 eps;
     check_within "segment_length s3" (segment_length s3) 14.14213 0.00001
  );

  "midpoint_segment" >:: (fun c ->
     let (x, y) = get_coords (midpoint_segment s3) in
       check_within "x coord of midpoint of segment s3" x 5.0 eps;
       check_within "y coord of midpoint of segment s3" y 5.0 eps
  );

  (* A.2 *)
  "make_rectangle" >:: (fun c ->
     let p1  = make_point (-1.0) 3.4 in
     let p2  = make_point 1.0 43.0 in
     let r   = make_rectangle p1 p2 in
     let rl  = rectangle_lower_segment r in
     let ru  = rectangle_upper_segment r in
     let rlf = rectangle_left_segment  r in
     let rrt = rectangle_right_segment r in
     let rp  = rectangle_perimeter r in
     let ra  = rectangle_area r in
       begin
         check_within "segment length rl 1" (segment_length rl) 2.0 eps;
         check_within "segment length ru 1" (segment_length ru) 2.0 eps;
         check_within "segment length rlf 1" (segment_length rlf) 39.6 eps;
         check_within "segment length rrt 1" (segment_length rrt) 39.6 eps;
         check_within "rectangle perimeter 1" rp 83.2 eps;
         check_within "rectangle area 1" ra 79.2 eps;
       end
  );

  "make_rectangle2" >:: (fun c ->
     let x1  = -1.0 in
     let x2  = 1.0 in
     let y1  = 3.4 in
     let y2  = 43.0 in
     let r   = make_rectangle2 x1 y1 x2 y2 in
     let rl  = rectangle_lower_segment2 r in
     let ru  = rectangle_upper_segment2 r in
     let rlf = rectangle_left_segment2  r in
     let rrt = rectangle_right_segment2 r in
     let rp  = rectangle_perimeter2 r in
     let ra  = rectangle_area2 r in
       begin
         check_within "segment length rl 2" (segment_length rl) 2.0 eps;
         check_within "segment length ru 2" (segment_length ru) 2.0 eps;
         check_within "segment length rlf 2" (segment_length rlf) 39.6 eps;
         check_within "segment length rrt 2" (segment_length rrt) 39.6 eps;
         check_within "rectangle perimeter 2" rp 83.2 eps;
         check_within "rectangle area 2" ra 79.2 eps;
       end
  );

  (* A.3: no tests *)

  (* A.4 *)

  "integer pairs" >:: (fun c ->
     assert_equal  (pow 5 7) 78125;
     assert_equal  (pow 7 5) 16807;
     assert_equal  (int_log 5 78125) 7;
     assert_equal  (int_log 7 16807) 5;
     assert_equal  (make_pairi 5 7) 69984;
     assert_equal  (firsti (make_pairi 5 7)) 5;
     assert_equal  (secondi (make_pairi 5 7)) 7;
     assert_equal  (make_pairi 7 5) 31104;
     assert_equal  (firsti (make_pairi 7 5)) 7;
     assert_equal  (secondi (make_pairi 7 5)) 5
  );

  (* A.5 *)

  "unary integers" >:: (fun c ->
     assert_equal (prev [()]) [];
     assert_equal (prev [(); (); (); (); ()]) [(); (); (); ()];
     assert_equal (integer_to_unary 0) [];
     assert_equal (integer_to_unary 1) [()];
     assert_equal (integer_to_unary 10) [(); (); (); (); (); (); (); (); (); ()];
     assert_equal (unary_to_integer [(); (); (); (); (); (); (); (); ()]) 9;
     assert_equal (unary_to_integer [(); (); ()]) 3;
     assert_equal (unary_to_integer [()]) 1;
     assert_equal (unary_to_integer []) 0;
     assert_equal (unary_add [(); (); ()] []) [(); (); ()];
     assert_equal (unary_add [] [(); (); ()]) [(); (); ()];
     assert_equal (unary_add [(); ()] [(); (); ()]) [(); (); (); (); ()];
     assert_equal (unary_to_integer 
       (unary_add (integer_to_unary 1001) (integer_to_unary 65535))) 66536;
  );

  "unary integers 2" >:: (fun c ->
     assert_equal (prev' (Succ Zero)) Zero;
     assert_equal (prev' (Succ (Succ (Succ (Succ (Succ Zero)))))) 
       (Succ (Succ (Succ (Succ Zero))));
     assert_equal (integer_to_unary' 0) Zero;
     assert_equal (integer_to_unary' 1) (Succ Zero);
     assert_equal (integer_to_unary' 10) 
       (Succ (Succ (Succ (Succ (Succ 
         (Succ (Succ (Succ (Succ (Succ Zero))))))))));
     assert_equal (unary_to_integer' 
       (Succ (Succ (Succ (Succ (Succ 
         (Succ (Succ (Succ (Succ Zero)))))))))) 9;
     assert_equal (unary_to_integer' (Succ (Succ (Succ Zero)))) 3;
     assert_equal (unary_to_integer' (Succ Zero)) 1;
     assert_equal (unary_to_integer' Zero) 0;
     assert_equal (unary_add' (Succ (Succ (Succ Zero))) Zero) 
       (Succ (Succ (Succ Zero)));
     assert_equal (unary_add' Zero (Succ (Succ (Succ Zero)))) 
       (Succ (Succ (Succ Zero)));
     assert_equal (unary_add' (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))) 
       (Succ (Succ (Succ (Succ (Succ Zero)))));
     assert_equal (unary_to_integer'
       (unary_add' (integer_to_unary' 1001) (integer_to_unary' 65535))) 66536;
  );

  (* A.6: no tests *)

  (* A.7: no tests *)

  (*** Part B. ***)

  (* MAYBE-TODO: add tests for:
   * left_branch, right_branch, branch_length, branch_structure
   * branch_weight1, branch_weight2
   *
   * Also all primed functions: left_branch' etc.
   * (same tests)
   *
   * TODO: Label all tests!
   *)

  "is_balanced, total_weight1, total_weight2" >:: (fun c ->
     assert_bool  "is_balanced 1" (is_balanced m0);
     assert_bool  "is_balanced 2" (is_balanced m1);
     assert_bool  "is_balanced 3" (not (is_balanced m2));
     assert_bool  "is_balanced 4" (not (is_balanced m3));
     assert_equal (total_weight1 m0) 2;
     assert_equal (total_weight1 m1) 7;
     assert_equal (total_weight1 m2) 601;
     assert_equal (total_weight1 m3) 1202;
     assert_equal (total_weight2 m0) 2;
     assert_equal (total_weight2 m1) 7;
     assert_equal (total_weight2 m2) 601;
     assert_equal (total_weight2 m3) 1202
  );

  "is_balanced', total_weight'" >:: (fun c ->
     assert_bool  "is_balanced' 1" (is_balanced' m0');
     assert_bool  "is_balanced' 2" (is_balanced' m1');
     assert_bool  "is_balanced' 3" (not (is_balanced' m2'));
     assert_bool  "is_balanced' 4" (not (is_balanced' m3'));
     assert_equal (total_weight' m0') 2;
     assert_equal (total_weight' m1') 7;
     assert_equal (total_weight' m2') 601;
     assert_equal (total_weight' m3') 1202;
  );

  "square_tree, square_tree'" >:: (fun c ->
     assert_equal (square_tree (Tree [])) (Tree []);
     assert_equal (square_tree' (Tree [])) (Tree []);
     assert_equal (square_tree tree1) tree2;
     assert_equal (square_tree' tree1) tree2
  );

  "square_tree''" >:: (fun c ->
     let square_tree'' tree = tree_map (fun n -> n * n) tree in
       begin
         assert_equal (square_tree'' (Tree [])) (Tree []);
         assert_equal (square_tree'' tree1) tree2;
         assert_equal (square_tree'' tree1) (square_tree tree1);
         assert_equal (square_tree'' tree2) (square_tree tree2)
       end
  );

  (*** Part C. ***)

  "simplify" >:: (fun c ->
     assert_equal (simplify (Int 42)) (Int 42);
     assert_equal (simplify (Var "x")) (Var "x");
     assert_equal (simplify (Add (Int 32, Int 41))) (Int 73);
     assert_equal (simplify (Add (Add (Int 1, Int 2), Add (Int 3, Int 4))))
                  (Int 10);
     assert_equal (simplify (Add (Mul (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 14);
     assert_equal (simplify (Mul (Mul (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 24);
     assert_equal (simplify (Mul (Add (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 36);
     assert_equal (simplify (Pow (Int 0, 0))) (Int 1);
     assert_equal (simplify (Pow (Int 10, 2))) (Int 100);
     assert_equal (simplify (Pow (Add (Int 1, Int 2), 2))) (Int 9);
     assert_equal (simplify (Add (Var "x", Int 0))) (Var "x");
     assert_equal (simplify (Add (Int 0, Var "x"))) (Var "x");
     assert_equal (simplify (Mul (Int 0, Var "y"))) (Int 0);
     assert_equal (simplify (Mul (Var "y", Int 0))) (Int 0);
     assert_equal (simplify (Mul (Int 1, Var "z"))) (Var "z");
     assert_equal (simplify (Mul (Var "z", Int 1))) (Var "z");
     assert_equal (simplify (Pow (Var "x", 0))) (Int 1);
     assert_equal (simplify (Pow (Var "x", 1))) (Var "x");
     assert_equal (simplify (Pow (Add (Var "x", Int 0), 1))) (Var "x");
     assert_equal 
       (simplify (Add (Add (Var "x", Int 0), Mul (Var "y", Int 0)))) 
       (Var "x")
  );

  "derivative" >:: (fun c ->
     assert_equal (derivative "x" (Int 10)) (Int 0);
     assert_equal (derivative "x" (Var "x")) (Int 1);
     assert_equal (derivative "x" (Var "y")) (Int 0);
     assert_equal (derivative "x" (Add (Var "x", Var "x"))) (Int 2);
     assert_equal (derivative "x" (Add (Add (Var "x", Var "x"), Var "x"))) 
                  (Int 3);
     assert_equal (derivative "x" (Mul (Var "x", Int 42))) (Int 42);
     assert_equal (derivative "x" (Mul (Var "x", Var "y"))) (Var "y");
     assert_equal (derivative "z" (Mul (Var "x", Var "y"))) (Int 0);
     assert_equal 
       (derivative "x" (Mul (Pow (Var "x", 2), Mul (Int 3, Var "x"))))
       (Add (Mul (Mul (Int 2, Var "x"), Mul (Int 3, Var "x")),
             Mul (Pow (Var "x", 2), Int 3)));
     assert_equal (derivative "x" (Pow (Var "y", 1))) (Int 0);
     assert_equal (derivative "x" (Pow (Var "x", 1))) (Int 1);
     assert_equal (derivative "x" (Pow (Var "x", 2))) 
                  (Mul ((Int 2), (Var "x")));
     assert_equal (derivative "x" (Pow (Mul (Int 3, Var "x"), 3)))
                  (Mul 
                    (Mul 
                      (Int 3, 
                        Pow (Mul (Int 3, Var "x"), 2)),
                      Int 3));
     assert_equal (derivative "x" (Add (Mul (Int 4, Pow (Var "x", 3)), 
                                        Mul (Int 6, Pow (Var "x", 2)))))
                  (Add (Mul (Int 4, Mul (Int 3, Pow (Var "x", 2))),
                        Mul (Int 6, Mul (Int 2, Var "x"))))

  );
]

let _ = run_test_tt_main all_tests

