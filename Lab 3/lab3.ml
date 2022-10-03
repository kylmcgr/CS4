(* A.1 *)
let rec last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [i] -> [i]
  | h :: t -> last_sublist t

(* A.2 *)
let reverse lst =
  let rec iter lst rev = 
    match lst with
    | [] -> rev
    | h :: t -> iter t (h :: rev)
  in iter lst []

(* A.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: (square_list t)

let square_list2 items = 
  List.map (fun x -> x * x) items

(* A.4 *)
(*
This first code produces the answer list in the 
reverse order because the squared element is 
added to the front of the answer list. Since we 
recursively do this to the head of the input list 
we will have answer list being created in reverse.

The :: operator only takes a value then a list 
and returns a new list with the value at the 
front of the list. We can't use it to add 
elements to the end of a list.

let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [h * h])
  in iter items []
This function would not be efficient because the 
@ operator is much less efficient than the :: 
operator (O(size of first list) vs O(1))
*)

(* A.5 *)
let rec count_negative_numbers = function
  | [] -> 0
  | h :: t -> count_negative_numbers t + 
    if h < 0 then 1 else 0

(* A.6 *)
let rec power_of_two_list n = 
  let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | b -> a * pow a (b - 1)
  in
  let rec iter n answer =
    match n with
      | 0 -> answer
      | _ -> iter (n - 1) ((pow 2 (n - 1)) :: answer)
  in iter n []

(* A.7 *)
let rec prefix_sum lst = 
  let rec iter lst sum =
    match lst with
      | [] -> []
      | h :: t -> (h + sum) :: (iter t (h + sum))
  in iter lst 0

(* A.8 *)
let deep_reverse lst =
  let rec iter lst rev = 
    match lst with
    | [] -> rev
    | h :: t -> iter t ((reverse h) :: rev)
  in iter lst []

(* A.9 *)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let rec deep_reverse_nested lst =
  match lst with
  | Value lst -> Value lst
  | List lst -> 
    let rec iter lst rev = 
      match lst with
      | [] -> rev
      | h :: t -> iter t ((deep_reverse_nested h) :: rev)
    in List (iter lst [])
    

(* B.1 *)
let rec quicksort lst cmp =
  match lst with
  | [] -> []
  | h :: t -> 
    (quicksort (List.filter (fun x -> not (cmp h x)) t) cmp)
  @ [h] @ (quicksort(List.filter (fun x -> cmp h x) t) cmp)

(* B.2 *)
(*
Quicksort is an instance of generative because we 
are making recursive calls of lists that we 
create from filtered subsets of the inputs list.
*)

(* B.3 *)
(*
Ben get rid of the base case where there is a 
single element which results in looping 
recursion. This is because the single element 
will be put into the odd half list and sent to 
another iteration of the mergesort infinitely.
*)

(* B.4 *)
let rec insert_in_order new_result a_list cmp =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(*
This recursion is structual recursion because we 
are making recursive calls on the tail of our 
lists without changing it and the computations 
happen when the recursive calls get resolved.
*)

(* C.1 *)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun x -> h :: x) rest)

(*
We can see that this works because if we already 
have all the subsets for a list and then we add a 
new element to this list, the only new subsets 
that are created are all the existing subsets 
with the new element added to them. This is what 
we do here, take all the subsets of the list 
minus an element and take these subsets as well 
as these subsets with the element added to each 
of them.
*)

(* C.2 *)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> 1 + r) 0 sequence

(* C.3 *)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)

(* C.4 *)
let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h1 :: t1, h2 :: t2) -> (f h1 h2) :: (map2 f t1 t2)

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun x -> dot_product v x) m

let transpose mat = accumulate_n (fun x r -> x :: r) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (matrix_times_vector cols) m