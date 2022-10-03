(* A.1 *)
(*
1. int = 10
2. float = 10.
3. int = 12
4. Error: This expression has type float but an 
expression was expected of type int
This error occured becuase we used the integer
addition operator with floats.
5. Error: This expression has type int but an 
expression was expected of type float
This error occured beacuse we used the float 
addition operator with integers.
6. Error: This expression has type float but an 
expression was expected of type int
This error occured beacuse we used the integer 
addition operator with a float as an argument.
7. Error: This expression has type int but an 
expression was expected of type float
This error occured beacuse we used the float 
addition operator with an integer as an argument.
8. float = 7.2
9. int = 5
10. int = 7
11. val a : int = 3
12. val b : int = 4
13. bool = false
14. bool = true
15. bool = false
This is different from the previous expression 
because single equals checks if they are the same 
value and double equals checks if they are the 
same object in memory.
16. (int * int * int) list = [(1, 2, 3)]
17. (int * int * int) list = [(1, 2, 3)]
We get this result because we use commas instead 
of semicolons so this creates a list with a 
single tuple because of implicit parentheses.
18. int = 4
19. Error: Syntax error
This error occured because "and" is not an 
operator that it recognizes.
20. int = 6
21. int = 4
This is because ocaml interprets the + 2 as a 
part of the else statement.
22. int = 6
23. This expression has type int but an 
expression was expected of type unit because 
it is in the result of a conditional with no 
else branch.
If there is a conditional with no else branch, 
the else branch is assumed to have type unit, so
we get a type error for having a different type.
*)

(* A.2 *)
let sum_of_squares_of_two_largest a b c =
  if a > b && b > c then 
    a * a + b * b
  else if a > b then
    a * a + c * c
  else if a > c then
    a * a + b * b
  else
    b * b + c * c

(* A.3 *)
(*
The following function outputs a plus the 
absolute value of b, so if b is greater than 0 it 
will output a + b and if b is less than 0 it will
outpur a - b.
*)

(* B.1 *)
(*
In applicative order evaluation, the arguments 
to the function are evaluated before the if 
statement is evaluated so we will get stuck in 
infinite recursion of trying to evaluate p ().
In normal order evaluation, the arguments aren't 
evaluated until needed so p () will never be 
evaluated as we will reach the if statement and 
just return 0 as x is 0.
*)

(* B.2 *)
(*
Alyssa will run into infinite recursion because 
the new if is a function, so all of its arguments 
will be evaluated. Unlike an if where it checks 
the condition then decides to evaluate the then 
or else, the condition and both arguments are all 
evaluated, so sqrt_iter will recursively evaluate 
infinitely.
*)

(* B.3.1 *)
(*
add_a is a recursive process because it will need 
the result from the recursive calls to perform 
more operations. add_b is an iterative process
because it uses tail recursion so we are done 
with all operations when the last recursive call
finishes.
*)

(* B.3.2 *)
(*
let rec add_a a b =
  if a = 0
    then b
    else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
  fun a b ->
    if a = 0
      then b
      else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

  fun a b ->
    if a = 0
      then b
      else inc (add_a (dec a) b)

Evaluate (add_a 2 5)
  evaluate 2 -> 2
  evaluate 5 -> 5
  evaluate add_a -> fun a b -> if a = 0 then b else inc (add_a (dec a) b)
  apply (fun a b -> if ...) to 2, 5
    substitute 2 for a, 5 for b in (if ...)
      -> if 2 = 0 then 5 else inc (add_a (dec 2) 5)
      if is a special form, so evaluate the first operand:
        evaluate (2 = 0)
          evaluate 2 -> 2
          evaluate 0 -> 0
          evaluate = -> =
          apply = to 2, 0 -> false
      first argument of if is false, so evaluate the third operand:
        evaluate (inc (add_a (dec 2) 5))
          evaluate (add_a (dec 2) 5)
            evaluate (dec 2)
              evaluate 2 -> 2
              evaluate dec -> dec
              apply dec to 2 -> 1
            evaluate 5 -> 5
            evaluate add_a -> fun a b -> if ...
            apply (fun a b -> if ...) to 1, 5
              substitute 1 for a, 5 for b in (if ...)
                -> if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                if is a special form, so evaluate the first operand:
                  evaluate (1 = 0)
                    evaluate 1 -> 1
                    evaluate 0 -> 0
                    evaluate = -> =
                    apply = to 1, 0 -> false
                first argument of if is false, so evaluate the third operand:
                  evaluate (inc (add_a (dec 1) 5))
                    evaluate (add_a (dec 1) 5)
                      evaluate (dec 1)
                          evaluate 1 -> 1
                          evaluate dec -> dec
                          apply dec to 1 -> 0
                      evaluate 5 -> 5
                      evaluate add_a -> fun a b -> if ...
                        apply (fun a b -> if ...) to 0, 5
                          substitute 0 for a, 5 for b in (if ...)
                            -> if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                            if is a special form, so evaluate the first operand:
                              evaluate (0 = 0)
                                evaluate 0 -> 0
                                evaluate 0 -> 0
                                evaluate = -> =
                                apply = to 0, 0 -> true
                            first argument of if is true, so evaluate the second operand:
                              evaluate 5 -> 5
                    evaluate inc -> inc
                    apply inc to 5 -> 6
          evaluate inc -> inc
          apply inc to 6 -> 7
          result: 7
*)

(* B.3.3 *)
(*
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
  >>>evaluate 2 -> 2
  >>>evaluate 5 -> 5
  >>>evaluate add_b -> fun a b -> if a = 0 then b else add_b (dec a) (inc b)
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        >>>evaluate 2 -> 2
        >>>evaluate 0 -> 0
        >>>evaluate = -> =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
          >>>evaluate 2 -> 2
          >>>evaluate dec -> dec
          apply dec to 2 -> 1
        evaluate (inc 5)
          >>>evaluate 5 -> 5
          >>>evaluate inc -> inc
          apply inc to 5 -> 6
        >>>evaluate add_b -> fun a b -> if ...
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              >>>evaluate 1 -> 1
              >>>evaluate 0 -> 0
              >>>evaluate = -> =
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
                >>>evaluate 1 -> 1
                >>>evaluate dec -> dec
                apply dec to 1 -> 0
              evaluate (inc 6)
                >>>evaluate 6 -> 6
                >>>evaluate inc -> inc
                apply inc to 6 -> 7
              >>>evaluate add_b -> fun a b -> if ...
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                    >>>evaluate 0 -> 0
                    >>>evaluate 0 -> 0
                    >>>evaluate = -> =
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                  >>>evaluate 7 -> 7
                  result: 7
*)

(* C.1 *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

(* C.1.a *)
let e_term n =
  1. /. float_of_int (factorial n)

(* C.1.b *)
let rec e_approximation n =
  if n = 0 then 
    e_term 0
  else
    e_term n +. e_approximation (n-1)

(* C.1.c *)
(*
e_approximation 20;;
- : float = 2.71828182845904553
 exp 1.0 ;;
- : float = 2.71828182845904509
*)

(* C.1.d *)
(*
 e_approximation 100;;
- : float = infinity
This happens because factorial gets bigger than 
the maximum integer size so it wraps around and 
then becomes 0, so the e_term will be infinity.
*)

(* C.2 *)
let rec is_even a = 
  if a = 0 then true else is_odd (a-1)
and is_odd b = 
  if b = 0 then false else is_even (b-1)

(* C.3 *)
let rec f_rec n =
  if n < 3
    then 
      n 
    else 
      f_rec (n-1) + 2 * f_rec (n-2) + 3 * f_rec (n-3)

let rec f_iter_helper a b c x n =
  if x > n
    then 
      c 
    else if x < 3 then
      f_iter_helper b c x (x + 1) n
    else
      f_iter_helper b c (c + 2 * b + 3 * a) (x + 1) n

let f_iter n =
  f_iter_helper 0 0 0 1 n

(* C.4 *)
let rec pascal_coefficient r i =
  match i with
  _ when i < 1 || i > r -> failwith "invalid arguments"
	| x when x = r || x = 1 -> 1
	| _ -> pascal_coefficient (r-1) (i-1) + pascal_coefficient (r-1) (i)


