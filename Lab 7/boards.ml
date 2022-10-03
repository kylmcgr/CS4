(* Sample Klotski boards, for testing. *)
(* We also provide the minimum number of moves needed to solve the boards. *)

let board0 =
  "aabb" ^
  ".cc." ^
  "ddee" ^
  "ffgh" ^
  "ffij"

let board0_solve = 6

let board1 =
  ".aa." ^
  "bbcc" ^
  "deff" ^
  "gghi" ^
  "gghi"

let board1_solve = 9
  
let board2 =
  "aabc" ^
  "aabc" ^
  "defg" ^
  "dhhg" ^
  "...."

let board2_solve = 18

let board3 =
  "...." ^
  "abbc" ^
  "abbc" ^
  "deef" ^
  "dggf"

let board3_solve = 27

let board4 =
  "abbc" ^
  "defg" ^
  "dhhg" ^
  "ihhj" ^
  ".kk."

let board4_solve = 37

let board5 =
  "abbc" ^
  "abbc" ^
  "defg" ^
  "hiij" ^
  "h..j"

let board5_solve = 66

(* The "classic" Klotski board. *)
let board6 =
  "abbc" ^
  "abbc" ^
  "deef" ^
  "dghf" ^
  "i..j"

(* Wikipedia says that this takes 81 moves.  
 * However, that is with a different criteria for counting moves. *)
let board6_solve = 90

let board7 =
  "abcd" ^
  "eefd" ^
  "eefg" ^
  "hiig" ^
  "jj.."

let board7_solve = 150

let boards = 
  [| board0; board1; board2; board3; board4; board5; board6; board7 |]

let boards_solve =
  [|
     board0_solve;
     board1_solve;
     board2_solve;
     board3_solve;
     board4_solve;
     board5_solve;
     board6_solve;
     board7_solve
  |]

