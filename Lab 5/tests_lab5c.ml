(* Tests for lab5, part C. *)

open Lab5

let print msg x = 
  begin
    Printf.printf "\nExample: (%s) :\n\n" msg ;
    Printf.printf "    %s\n\n" (show x)
  end 


(* Definitions for problem C.2 *)

let n0  = make_number 0
let n1  = make_number 1
let n2  = make_number 2
let x   = make_variable "x"
let s1  = make_sum n1 n2
let s2  = make_sum n1 n0
let s3  = make_sum x n1
let s4  = make_sum (make_variable "y") (make_number 4)
let p1  = make_product n2 n2
let p2  = make_product x n0
let p3  = make_product x n2
let p4  = make_product x s1
let sl1 = make_sum p3 (make_sum p4 s3)
let p5  = make_product n2 p4
let p6  = make_product x s3
let ap1 = make_sum p3 p4
let pa1 = make_product s3 s4
let pl1 = make_product s2 (make_product s4 sl1)

let _ =
  begin
    print "p3" p3 ;
    print "p3#derive \"x\""  (p3#derive  "x") ;  (* 2 *)
    print "p3#derive \"y\""  (p3#derive  "y") ;  (* 0 *)
    print "p6" p6 ;
    print "p6#derive \"x\""  (p6#derive  "x") ;  (* ((x + 1) + x) *)
    print "pl1" pl1 ;
    print "pl1#derive \"x\"" (pl1#derive "x") ;  (* ((y + 4) * 6) *)
    print "pl1#derive \"y\"" (pl1#derive "y") ;  (* ((x * 2) + ((x * 3) + (x + 1))) *)
    print "s3" s3 ;
    print "s3#derive \"x\""  (s3#derive  "x") ;  (* 1 *)
  end

(* g represents 5*x + x*y + 7*y *)

let g =
  make_sum 
    (make_product 
      (make_number 5)
      (make_variable "x"))
    (make_sum
      (make_product 
        (make_variable "x")
        (make_variable "y"))
      (make_product 
        (make_number 7)
        (make_variable "y")))

let _ = 
  begin
    print "g" g ;
    print "g#evaluate \"x\" 2" (g#evaluate "x" 2) ;
    print "g#evaluate \"y\" 3" (g#evaluate "y" 3) ;
    print "(g#evaluate \"x\" 2)#evaluate \"y\" 3" 
      ((g#evaluate "x" 2)#evaluate "y" 3) ;
    print "g#derive \"x\"" (g#derive "x") ;
    print "g#derive \"y\"" (g#derive "y") ;
    print "g#derive \"z\"" (g#derive "z") ;
    print "((g#derive \"x\")#evaluate \"x\" 2)#evaluate \"y\" 3" 
      (((g#derive "x")#evaluate "x" 2)#evaluate "y" 3) ;
  end


(* f = x^3*y + 3*x^2*y^2 + y^2 + 2 *)

let f =
  make_sum
   (make_product 
    (make_variable "x")
    (make_product 
     (make_variable "x")
     (make_product
      (make_variable "x")
      (make_variable "y"))))
   (make_sum
    (make_product 
     (make_number 3)
     (make_product
      (make_variable "x")
      (make_product
       (make_variable "x")
       (make_product
        (make_variable "y")
        (make_variable "y")))))
    (make_sum
     (make_product 
      (make_variable "y")
      (make_variable "y"))
     (make_number 2)))

let _ =
  begin
    print "f" f ;
    print "differentiate f \"x\"" (differentiate f "x") ;
    print "(evaluate f \"x\" 3)" (evaluate f "x" 3) ;
    print "(evaluate (evaluate f \"x\" 3) \"y\" 4)" 
      (evaluate (evaluate f "x" 3) "y" 4) ;
    let dfdx = differentiate f "x" in
      print "dfdx" dfdx ;
      print "(evaluate (evaluate dfdx \"x\" 3) \"y\" 4)" 
        (evaluate (evaluate dfdx "x" 3) "y" 4) ;
  end
