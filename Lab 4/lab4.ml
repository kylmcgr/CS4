(* A.1 *)
type point = { x : float; y : float }
type segment = { startp : point; endp : point }

let midpoint_segment { startp; endp } =
  let x = (startp.x +. endp.x) /. 2. in
    let y = (startp.y +. endp.y) /. 2. in
      { x; y }

let segment_length { startp; endp } =
  sqrt ((startp.x -. endp.x)**2. +. (startp.y -. endp.y)**2.)

let print_point { x; y } =
  Printf.printf "(%g, %g)" x y

let make_point x y = { x; y }

let get_coords { x; y } = (x, y)

let make_segment startp endp = {startp; endp}

let get_points { startp; endp } = (startp, endp)

(* A.2 *)
type rectangle = { ll : point; ur : point }

let rectangle_lower_segment { ll : point; ur : point } =
  make_segment ll (make_point ur.x ll.y)

let rectangle_upper_segment { ll : point; ur : point } =
  make_segment ur (make_point ll.x ur.y)

let rectangle_left_segment { ll : point; ur : point } =
  make_segment ll (make_point ll.x ur.y)

let rectangle_right_segment { ll : point; ur : point } =
  make_segment ur (make_point ur.x ll.y)

let rectangle_perimeter rect =
  segment_length (rectangle_lower_segment rect) +.
  segment_length (rectangle_upper_segment rect) +.
  segment_length (rectangle_left_segment rect) +.
  segment_length (rectangle_right_segment rect)

let rectangle_area rect =
segment_length (rectangle_lower_segment rect) *.
segment_length (rectangle_left_segment rect)

type rectangle2 = { lx : float; ly : float; ux : float; uy : float }

let rectangle_lower_segment2 { lx; ly; ux; uy } =
  make_segment (make_point lx ly) (make_point ux ly)

let rectangle_upper_segment2 { lx; ly; ux; uy } =
  make_segment (make_point lx uy) (make_point ux uy)

let rectangle_left_segment2 { lx; ly; ux; uy } =
  make_segment (make_point lx ly) (make_point lx uy)

let rectangle_right_segment2 { lx; ly; ux; uy } =
  make_segment (make_point ux ly) (make_point ux uy)

let rectangle_perimeter2 rect =
  segment_length (rectangle_lower_segment2 rect) +.
  segment_length (rectangle_upper_segment2 rect) +.
  segment_length (rectangle_left_segment2 rect) +.
  segment_length (rectangle_right_segment2 rect)

let rectangle_area2 rect =
segment_length (rectangle_lower_segment2 rect) *.
segment_length (rectangle_left_segment2 rect)

let make_rectangle ll ur = { ll; ur }

let make_rectangle2 lx ly ux uy = { lx; ly; ux; uy }

(* A.3 *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)
(*
1. first (make_pair x y) => first (fun m -> m x y)
first (fun m -> m x y) => (fun m -> m x y) (fun x y -> x)
(fun m -> m x y) (fun x y -> x) => (fun x y -> x) x y
(fun x y -> x) x y => x
2. second (make_pair 1 2)
let make_pair x y = fun m -> m x y
Desugar this to:
let make_pair x y = fun x y -> (fun m -> m x y)
Bind the name "make_pair" to the value:
  fun x y -> (fun m -> m x y)

let second z = z (fun x y -> y)
Desugar this to:
let second z = fun z -> (z (fun x y -> y))
Bind the name "second" to the value:
  fun z -> (z (fun x y -> y))

Evaluate second (make_pair 1 2)
  evaluate (make_pair 1 2)
    evaluate 1 -> 1
    evaluate 2 -> 2
    evaluate make_pair -> fun x y -> (fun m -> m x y)
    apply fun x y -> (fun m -> m x y) to 1, 2
      substitute 1 for x, 2 for y in fun m -> m x y
        -> (fun m -> m 1 2)
  evaluate second -> fun z -> (z (fun x y -> y))
  apply fun z -> (z (fun x y -> y)) to (fun m -> m 1 2)
    substitute (fun m -> m 1 2) for z in (z (fun x y -> y))
      -> (fun m -> m 1 2) (fun x y -> y)
      evaluate (fun m -> m 1 2) (fun x y -> y)
        evaluate (fun x y -> y) -> (fun x y -> y)
        evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
        apply (fun m -> m 1 2) to (fun x y -> y)
          substitute (fun x y -> y) for m in (m 1 2)
            -> ((fun x y -> y) 1 2)
            evaluate (fun x y -> y) 1 2
              evaluate 1 -> 1
              evaluate 2 -> 2
              evaluate (fun x y -> y) -> (fun x y -> y)
              apply (fun x y -> y) to 1, 2
                substitute 1 for x, 2 for y in (y)
                  -> 2
                result: 2
*)

(* A.4 *)
let rec pow a b = 
  match b with
  0 -> 1
  | 1 -> a
  | b -> a * pow a (b - 1)

let rec int_log a b = 
  match b with
    _ when b mod a <> 0 -> 0
    | 1 -> 0
    | b -> 1 + int_log a (b/a)

let make_pairi a b = (pow 2 a) * (pow 3 b)

let firsti z = int_log 2 z

let secondi z = int_log 3 z

(* A.5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev u = 
  match u with
    () :: t -> t
    | _ -> invalid_arg "cannot take prev of 0"
    
let rec integer_to_unary a =
  if a = 0 then
    zero
  else 
    succ (integer_to_unary (a - 1))

let rec unary_to_integer u =
  if is_zero u then
    0
  else 
    1 + unary_to_integer (prev u)

let rec unary_add u1 u2 =
  if is_zero u2 then
    u1
  else 
    unary_add (succ u1) (prev u2)

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' u = 
  match u with
    Succ t -> t
    | _ -> invalid_arg "cannot take prev of 0"

(*
There are no changes other than name changes.
*)

let rec integer_to_unary' a =
  if a = 0 then
    zero'
  else 
    succ' (integer_to_unary' (a - 1))

let rec unary_to_integer' u =
  if is_zero' u then
    0
  else 
    1 + unary_to_integer' (prev' u)

let rec unary_add' u1 u2 =
  if is_zero' u2 then
    u1
  else 
    unary_add' (succ' u1) (prev' u2)

(* A.6 *)
let zerof = fun s -> fun z -> z
let add1 n = fun s -> fun z -> s (n s z)
let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = (m s) ((n s) z)

let church_to_integer n = (n (fun z -> z + 1)) 0

(* A.7 *)
(*
val zerof : 'a -> 'b -> 'b
Desugar to : 'a -> ('b -> 'b)
val one : ('a -> 'b) -> 'a -> 'b
Desugar to : ('a -> 'b) -> ('a -> 'b)
val church_to_integer : ((int -> int) -> int -> 'c) -> 'c
Desugar to : ((int -> int) -> (int -> 'c)) -> 'c

When we evaluate church_to_integer zerof we have 
the (int -> int) from fun z -> z + 1 and the 
(int -> 'c) from (n (fun ...)) 0. Since n is the 
zerof function and it has type of 'a -> ('b -> 'b),
then (n (fun ...)) has type (int -> int) -> ('b -> 'b)
in this case and (n (fun ...)) 0 must have type 
(int -> int). Therefore, church_to_integer zerof 
must have type: ((int -> int) -> (int -> int)) -> int

When we evaluate church_to_integer one we have 
the (int -> int) from fun z -> z + 1 and the 
(int -> 'c) from (n (fun ...)) 0. Since n is the 
one function and it has type of ('a -> 'b) -> ('a -> 'b),
then (n (fun z -> z + 1)) has type 
(int -> int) -> (int -> int) in this case.
Therefore, church_to_integer zerof must have type:
((int -> int) -> (int -> int)) -> int
*)

(* B.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* B.1.a *)
let left_branch = function
  | Mobile (l, r) -> l
let right_branch = function
  | Mobile (l, r) -> r
let branch_length = function
  | Weight (l, w) -> l
  | Structure (l, m) -> l
let branch_structure = function
  | Weight (l, w) -> `Weight w
  | Structure (l, m) -> `Structure m

(* B.1.b *)
let rec branch_weight1 = function
  | Weight (l, w) -> w
  | Structure (l, m) -> total_weight1 m
  and total_weight1 = function
  | Mobile (l, r) -> (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b = 
  match branch_structure b with
    | `Weight w -> w
    | `Structure m -> total_weight2 m
  and total_weight2 m = 
  (branch_weight2 (left_branch m)) + (branch_weight2 (right_branch m))

(* B.1.c *)
let rec is_balanced m = 
  let is_balanced_branch b = 
    match branch_structure b with
      | `Weight w -> true
      | `Structure m -> is_balanced m
  in let l = left_branch m
    and r = right_branch m 
  in ((branch_length l * branch_weight2 l) 
  == (branch_length r * branch_weight2 r))
  && is_balanced_branch l && is_balanced_branch r

(* B.1.d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = { left; right }
let make_weight' l w = Branch' (l, Weight' (w))
let make_structure' l m = Branch' (l, Structure' (m))

let left_branch' m = m.left
let right_branch' m = m.right
let branch_length' = function
  | Branch' (l, Weight' (w)) -> l
  | Branch' (l, Structure' (m)) -> l
let branch_structure' = function
  | Branch' (l, Weight' (w)) -> `Weight w
  | Branch' (l, Structure' (m)) -> `Structure m

let rec branch_weight' b = 
  match branch_structure' b with
    | `Weight w -> w
    | `Structure m -> total_weight' m
  and total_weight' m = 
  (branch_weight' (left_branch' m)) + (branch_weight' (right_branch' m))

let rec is_balanced' m = 
  let is_balanced_branch' b = 
    match branch_structure' b with
      | `Weight w -> true
      | `Structure m -> is_balanced' m
  in let l = left_branch' m
    and r = right_branch' m 
  in ((branch_length' l * branch_weight' l) 
  == (branch_length' r * branch_weight' r))
  && is_balanced_branch' l && is_balanced_branch' r

(* B.2 *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree (Tree (l)) = 
  let rec square = function
  | [] -> []
  | Sub h :: t -> Sub (square_tree h) :: (square t)
  | Num h :: t -> Num (h*h) :: (square t)
in Tree (square l)

let rec square_tree' (Tree (l)) = 
  let square = function
  | Sub h -> Sub (square_tree h)
  | Num h -> Num (h*h)
in Tree (List.map square l)

(* B.3 *)
let rec tree_map f (Tree (l)) = 
  let func = function
  | Sub h -> Sub (tree_map f h)
  | Num h -> Num (f h)
in Tree (List.map func l)

let square_tree'' tree = tree_map (fun n -> n * n) tree

(* C.1 *)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e
  and simplify1 = function
    | Add (Int a, Int b) -> Int (a+b)
    | Mul (Int a, Int b) -> Int (a*b)
    | Pow (Int a, b) -> Int (pow a b)
    | Add (Int 0, a) -> simplify a
    | Add (a, Int 0) -> simplify a
    | Mul (Int 0, a) -> Int 0
    | Mul (a, Int 0) -> Int 0
    | Mul (Int 1, a) -> simplify a
    | Mul (a, Int 1) -> simplify a
    | Pow (a, 0) -> Int 1
    | Pow (a, 1) -> simplify a
    | Add (a, b) -> Add (simplify a, simplify b)
    | Mul (a, b) -> Mul (simplify a, simplify b)
    | Pow (a, b) -> Pow (simplify a, b)
    | Int a -> Int a
    | a -> a

(* C.2 *)
let rec derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d
  and deriv var = function
    | Int e -> Int 0
    | Var e when e = var -> Int 1
    | Var _ -> Int 0
    | Add (a, b) -> Add ((deriv var a), (deriv var b))
    | Mul (a, b) -> Add(Mul((deriv var a), b), Mul(a, (deriv var b)))
    | Pow (a, b) -> Mul(Mul(Int b, Pow (a, b - 1)), deriv var a)
