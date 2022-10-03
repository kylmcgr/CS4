(* A.1 *)
(*
The space complexity of this function will be 
O(n) because when we apply fib (n - 1) + 
fib (n - 2), we will evaluate fib (n - 1) for the 
n we are at and will not evaluate fib (n - 2) 
until we are done evaluating fib (n - 1). This 
means that the most pending computations will be 
the recursive evalutation and application of 
fib (n - 1) for O(n) space complexity.
*)

(* A.2 *)
(*
1. sine 12.15:
sine 12.15
  p sine 4.5
    p sine 1.5
      p sine 0.5
        p sine 0.1667
          p sine 0.0556
p will be applied 5 times.
2. The order of growth in space and number of 
steps of sine a is O(a). When 'a' grows by a 
factor of 3 then we use an order of 1 more space 
and number of steps, and O(a/3) = O(a).
*)

(* A.3.a *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
    0 -> 1
    | _ when is_even n -> square (fast_expt b (n / 2))
    | _ -> b * fast_expt b (n - 1)

(* A.3.b *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter n b a = 
    match n with
    0 -> a
    | _ when is_even n -> iter (n / 2) (square b) a
    | _ -> iter (n - 1) b (a*b)
  in iter n b 1

(* A.4 *)
let rec fast_mult a b =
  let is_even m = m mod 2 = 0 in
  let double m = 2 * m in
  let half m = m / 2 in
    match b with
    0 -> 0
    | _ when is_even b -> fast_mult (double a) (half b)
    | _ -> a + fast_mult a (b - 1)

(* A.5 *)
let ifast_mult b n =
  let is_even m = m mod 2 = 0 in
  let double m = 2 * m in
  let half m = m / 2 in
  let rec iter n b a = 
    match b with
    0 -> a
    | _ when is_even b -> iter (double n) (half b) a
    | _ -> iter n (b - 1) (n + a)
  in iter n b 0

(* A.6 *)
(*
The space complexity is O(log n) because the most 
amount of waiting recursive calls that we will 
have taking up space is dividing n by 2 until we 
get to 1, so log n. The time complexity is O(n) 
because when we divide n by 2, we run two 
recursive calls, so have a total of n calls.
*)

(* A.7 *)
(*
1. This is a linear recursive fuction because 
we need to recursively compute p0 and p1 to 
define the tuple we want to return, but we have 
just a single recursive call to n-1 each time.
2. The space and time complexity of this function 
are both O(n) because we will have n total calls 
to the recursive function each of constant time 
and each call will generate a single pending 
operation of assigning the tuple.
*)

(* B.1.a *)
(*
(fun x y -> x * (2 + y)) 20 (2 * 4)
*)

(* B.1.b *)
(*
(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
*)

(* B.1.c *)
(*
(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1 
*)

(* B.1.d *)
(*
(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1 
*)

(* B.2 *)
(*
let x = 2 * 10
and y = 3 + 4
in
  let y = 14 in
  let z = 22 in
    x * y * z

Desugar the outtermost let-and-in to:
(fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
    
Desugar the next let-in to:
(fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
    
Desugar the final let-in to:
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)

Evaluate (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
  evaluate (2 * 10) -> 20
  evaluate (3 + 4) -> 7
  evaluate fun x y -> (fun y -> (fun z -> x * y * z) 22) 14
    evaluate 14 -> 14
    evaluate fun y -> (fun z -> x * y * z) 22
      evaluate 22 -> 22
      evaluate fun z -> x * y * z
      apply (fun z -> x * y * z) to 22
        substitute 22 for z in (x * y * z) -> (x * y * 22)
        evaluate (x * y * 22) -> (x * y * 22)
    apply (fun y -> (x * y * 22)) to 14
      substitute 14 for y in (x * y * 22) -> (x * 14 * 22)
  apply (fun x y -> (x * 14 * 22)) to 20 7
    substitute 20 for x and 7 for y in (x * 14 * 22) -> (20 * 14 * 22)
evaluate (20 * 14) * 22
evaluate (20 * 14) -> 280
evaluate 22 -> 22
evaluate * -> *
apply * to 280 and 22 -> 6160
*)

(* B.3 *)
(*
(fun x y z -> x + y + z) 10 (x * 2) (y + 3)
The x, y, and z are all desugared to arguments to 
the function so x and y are not defined when they 
try to get applied.

We can change the and's to let-in's so that the 
y+3 is subsituted into the z and the x*2 is 
substituted into the y, and x has a value when it 
is applied.

let x = 10 in
let y = x * 2 in
let z = y + 3
in x + y + z
*)

open Num
let ni = num_of_int 

(* C.1 *)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (term a +/ result)
  in
    iter a (ni 0)

(* C.2 *)
let rec product_rec term a next b =
  if a >/ b
     then (ni 1)
     else term a */ (product_rec term (next a) next b)

let product_iter term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (term a */ result)
  in
    iter a (ni 1)

let factorial_rec n = 
  product_rec (fun x -> x) (ni 1) (fun n -> n +/ (ni 1)) n
     
let factorial_iter n = 
  product_iter (fun x -> x) (ni 1) (fun n -> n +/ (ni 1)) n

let pi_product n = 
  product_iter (fun x -> x // (x +/ (ni 1)) */ (x +/ (ni 2)) // (x +/ (ni 1)))
    (ni 2) (fun n -> n +/ (ni 2)) n
  
let pi_approx = float_of_num ((ni 4) */ (pi_product (ni 1000)))

(* C.3 *)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
     then null_value
     else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (combiner (term a) result)
  in
    iter a null_value

let sum term a next b = accumulate_iter ( +/ ) (ni 0) term a next b

let product term a next b = accumulate_iter ( */ ) (ni 1) term a next b

(* C.4 *)
let compose f g = fun x -> f (g x)

(* C.5 *)
let rec repeated f n = 
  if n = 0 then
    fun x -> x
  else compose f (repeated f (n - 1))

(* C.6 *)
let smooth dx f x = 
  (f (x +. dx) +. f x +. f (x -. dx)) /. 3.

let nsmoothed dx f n x = 
  (repeated (smooth dx) n f) x

(* D.1 *)
let is_prime n = 
  if n < 2 then false
  else 
    let is_divisible a b = a mod b = 0 in
    let rec iter p =
      match p with
      _ when p < 2 -> true
      | _ when is_divisible n p -> false
      | _ -> iter (p - 1)
    in iter (int_of_float (sqrt (float_of_int n)))
  
(* D.2 *)
let smallest_prime_factor n = 
  let is_divisible a b = a mod b = 0 in
  let rec iter p =
    match p with
    _ when p >= n -> failwith "invalid_arg"
    | _ when is_prime p && is_divisible n p -> p
    | _ -> iter (p + 1)
  in iter 2