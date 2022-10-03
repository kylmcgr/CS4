(* Interface file for lab2.ml *)

open Num

val fast_expt : int -> int -> int
val ifast_expt : int -> int -> int
val fast_mult : int -> int -> int
val ifast_mult : int -> int -> int
val isum : (num -> num) -> num -> (num -> num) -> num -> num
val product_rec : (num -> num) -> num -> (num -> num) -> num -> num
val factorial_rec : num -> num
val pi_product : num -> num
val pi_approx : float
val product_iter : (num -> num) -> num -> (num -> num) -> num -> num
val factorial_iter : num -> num
val accumulate_rec :
  (num -> num -> num) -> num -> (num -> num) -> num -> (num -> num) -> num -> num
val accumulate_iter :
  (num -> num -> num) -> num -> (num -> num) -> num -> (num -> num) -> num -> num
val sum : (num -> num) -> num -> (num -> num) -> num -> num
val product : (num -> num) -> num -> (num -> num) -> num -> num
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val repeated : ('a -> 'a) -> int -> 'a -> 'a
val smooth : float -> (float -> float) -> float -> float 
val nsmoothed : float -> (float -> float) -> int -> float -> float
val is_prime : int -> bool
val smallest_prime_factor : int -> int

