(* Interface file for lab4.ml *)

(*** Part A. ***)

(* A.1 *)

type point
type segment
val make_point : float -> float -> point
val get_coords : point -> float * float
val make_segment : point -> point -> segment
val get_points : segment -> point * point
val midpoint_segment : segment -> point 
val segment_length : segment -> float
val print_point : point -> unit

(* A.2 *)

type rectangle
val rectangle_lower_segment : rectangle -> segment
val rectangle_upper_segment : rectangle -> segment
val rectangle_left_segment : rectangle -> segment
val rectangle_right_segment : rectangle -> segment

type rectangle2
val rectangle_lower_segment2 : rectangle2 -> segment
val rectangle_upper_segment2 : rectangle2 -> segment
val rectangle_left_segment2 : rectangle2 -> segment
val rectangle_right_segment2 : rectangle2 -> segment
val rectangle_perimeter : rectangle -> float
val rectangle_perimeter2 : rectangle2 -> float
val rectangle_area : rectangle -> float
val rectangle_area2 : rectangle2 -> float
val make_rectangle : point -> point -> rectangle
val make_rectangle2 : float -> float -> float -> float -> rectangle2

(* A.3 *)

val make_pair : 'a -> 'b -> ('a -> 'b -> 'c) -> 'c
val first : (('a -> 'b -> 'a) -> 'c) -> 'c
val second : (('a -> 'b -> 'b) -> 'c) -> 'c

(* A.4 *)

val pow : int -> int -> int
val int_log : int -> int -> int
val make_pairi : int -> int -> int
val firsti : int -> int
val secondi : int -> int

(* A.5 *)

val zero : unit list
val is_zero : unit list -> bool
val succ : unit list -> unit list
val prev : unit list -> unit list
val integer_to_unary : int -> unit list
val unary_to_integer : unit list -> int
val unary_add : unit list -> unit list -> unit list

type nat = Zero | Succ of nat
val zero' : nat
val is_zero' : nat -> bool
val succ' : nat -> nat
val prev' : nat -> nat
val integer_to_unary' : int -> nat
val unary_to_integer' : nat -> int
val unary_add' : nat -> nat -> nat

(* A.6 and A.7 *)

val zerof : 'a -> 'b -> 'b
val add1 : (('a -> 'b) -> 'c -> 'a) -> ('a -> 'b) -> 'c -> 'b
val one : ('a -> 'b) -> 'a -> 'b
val two : ('a -> 'a) -> 'a -> 'a
val three : ('a -> 'a) -> 'a -> 'a
val four : ('a -> 'a) -> 'a -> 'a
val five : ('a -> 'a) -> 'a -> 'a
val six : ('a -> 'a) -> 'a -> 'a
val seven : ('a -> 'a) -> 'a -> 'a
val eight : ('a -> 'a) -> 'a -> 'a
val nine : ('a -> 'a) -> 'a -> 'a
val ten : ('a -> 'a) -> 'a -> 'a
val add : ('a -> 'b -> 'c) -> ('a -> 'd -> 'b) -> 'a -> 'd -> 'c
val church_to_integer : ((int -> int) -> int -> 'a) -> 'a

(*** Part B. ***)

(* B.1 *)

type mobile = Mobile of branch * branch
and branch = Weight of int * int | Structure of int * mobile
val make_mobile : branch -> branch -> mobile
val make_weight : int -> int -> branch
val make_structure : int -> mobile -> branch

(* 1. *)

val left_branch : mobile -> branch
val right_branch : mobile -> branch
val branch_length : branch -> int
val branch_structure : branch -> [> `Structure of mobile | `Weight of int ]

(* 2. *)

val branch_weight1 : branch -> int
val total_weight1 : mobile -> int
val branch_weight2 : branch -> int
val total_weight2 : mobile -> int

(* 3. *)

val is_balanced : mobile -> bool

(* 4. *)

type mobile' = { left : branch'; right : branch'; }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'
val make_mobile' : branch' -> branch' -> mobile'
val make_weight' : int -> int -> branch'
val make_structure' : int -> mobile' -> branch'
val left_branch' : mobile' -> branch'
val right_branch' : mobile' -> branch'
val branch_length' : branch' -> int
val branch_structure' : branch' -> [> `Structure of mobile' | `Weight of int ]
val branch_weight' : branch' -> int
val total_weight' : mobile' -> int
val is_balanced' : mobile' -> bool

(* B.2 *)

type tree = Tree of elem list
and elem = Num of int | Sub of tree
val square_tree : tree -> tree
val square_tree' : tree -> tree

(* B.3 *)

val tree_map : (int -> int) -> tree -> tree

(*** Part C. ***)

(* C.1 *)

type expr =
    Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int

val simplify1 : expr -> expr

(* C.2 *)

val deriv : string -> expr -> expr

