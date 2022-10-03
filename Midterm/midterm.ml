(* name: Kyle McGraw *)
(* email: kmcgraw@caltech.edu *)

(* A.1 *)
let list_of_string s = 
  let rec iter str lst i = 
    if i < 0 then
      lst
    else
      iter str (str.[i] :: lst) (i - 1)
  in iter s [] (String.length s - 1)

(* A.2 *)
let remove_exact_matches lst1 lst2 = 
  let rec iter (l1, l2) = 
    match (l1, l2) with
      ([], _ :: _) | (_ :: _, []) -> 
        failwith "remove_exact_matches: lists of different lengths"
      | ([], []) -> ([], [])
      | (h1 :: t1, h2 :: t2) -> 
        let (t1', t2') = iter (t1, t2) in 
        if h1 = h2 then 
          ('_' :: t1', '_' :: t2')
        else
          (h1 :: t1', h2 :: t2')
  in iter (lst1, lst2)

let remove_exact_matches' lst1 lst2 = 
  let rec iter (l1, l2) (l1', l2') = 
    match (l1, l2) with
      ([], _ :: _) | (_ :: _, []) -> 
        failwith "remove_exact_matches: lists of different lengths"
      | ([], []) -> (List.rev l1', List.rev l2')
      | (h1 :: t1, h2 :: t2) -> 
        if h1 = h2 then 
          iter (t1, t2) ('_' :: l1', '_' :: l2')
        else
          iter (t1, t2) (h1 :: l1', h2 :: l2')
  in iter (lst1, lst2) ([], [])

(*
These worst case time complexity for these two 
functions are O(N) where N is the size of the 
lists. This is beacuse we iterate through each 
index of the lists together and at each spot, do 
constant operations of comparison or cons/car/cdr.
For the second function, we do List.rev which is 
also O(N), but because we only do it once for 
each list at the very end, it only adds O(2N) 
time for total O(3N) = O(N) time.
*)

(* A.3 *)
let find_and_remove_char chr lst = 
  let rec iter word newword found = 
    match word with
      [] when found -> Some (List.rev newword)
      | [] -> None
      | h :: t when h = chr && not found -> 
        iter t ('_' :: newword) true
      | h :: t -> iter t (h :: newword) found
  in iter lst [] false

(*
These worst case time complexity for this 
functions is O(N) where N is the size of the 
lists. This is beacuse we iterate through each 
letter of the word and call List.rev once for 
O(N + N) = O(N).
*)

(* A.4 *)
let get_matches target guess = 
  if List.length target <> List.length guess then
    failwith "get_matches: lists of different lengths"
  else
    let exact = remove_exact_matches target guess
    in let rec iter (l1, l2) = 
      match (l1, l2) with
        (_, []) -> []
        | (t1, h2 :: t2) ->
          if h2 = '_' then 
            'G' :: iter (t1, t2)
          else
            let t1' = find_and_remove_char h2 t1
            in match t1' with
              None -> 'B' :: iter (t1, t2)
              | Some a -> 'Y' :: iter (a, t2)
    in iter exact

(*
On the outer most level, we run remove_exact_matches
once which is O(N). We then iterate through every 
letter in the guess for N total iterations. For 
each iteration, worst case we call find_and_remove_char 
every time which has O(N) time. This gives a total 
time complexity of O(N + N^2) for O(N^2) time.
*)

(* A.5 *)
let get_letter_colors target guess = 
  let targetl = list_of_string target
    and guessl = List.map list_of_string guess
  in let matches = List.map (get_matches targetl) guessl
  in let guessmatches = List.combine guessl matches
  in let combined = 
    List.concat_map (fun (g, m) -> List.combine g m) guessmatches
  in let sorted = List.sort compare combined
  in let compareColor chr1 chr2 = 
    match (chr1, chr2) with
      ('G', _) | (_, 'G') -> 'G'
      | ('Y', _) | (_, 'Y') -> 'Y'
      | (_, _) -> 'B'
  in let removedupe lst (chr, color) = 
    match lst with
      ((h, hcolor) :: t) when h = chr -> 
        (h, compareColor color hcolor) :: t
      | _ -> (chr, color) :: lst
  in List.rev (List.fold_left removedupe [] sorted)

(* A.6 *)
let rec gray_codes n = 
  match n with
  _ when n < 1 -> invalid_arg "gray_codes"
  | 1 -> [[0]; [1]]
  | _ -> let g1 = gray_codes (n - 1)
    in (List.map (fun x -> 0 :: x) g1) 
    @ (List.rev (List.map (fun x -> 1 :: x) g1))
    

(* B *)
type tree =
  | Leaf
  | Node of int * int * tree * tree   (* depth, value, left/right subtrees *)

(* Depth of an AVL tree. *)
let depth = function
  | Leaf -> 0
  | Node (d, _, _, _) -> d

(* Extract the data value from a node. *)
let data = function
  | Leaf -> failwith "no data"
  | Node (_, v, _, _) -> v

(* Create a new node from two subtrees and a data value.
 * This assumes that the ordering invariant holds i.e.
 * that v is greater than any value in the left subtree
 * and is smaller than any value in the right subtree.  *)
let make_node v l r =
  let d = 1 + max (depth l) (depth r) in  (* compute the correct depth *)
    Node (d, v, l, r)

(* B.1 *)
let rec search n = function
  | Leaf -> false
  | Node (d, v, l, r) -> 
    if n = v then true
    else search n (if n < v then l else r)

(* B.2 *)
let left_rotate = function
  | Node (d, v, l, Node(d2, v2, rl, rr)) -> 
    make_node v2 (make_node v l rl) rr
  | _ -> failwith "can't left rotate"
    
let right_rotate = function
  | Node (d, v, Node (d2, v2, ll, lr), r) -> 
    make_node v2 ll (make_node v lr r)
  | _ -> failwith "can't right rotate"

(* B.3 *)
let rec insert v t =
  match t with
    | Leaf -> Node (1, v, Leaf, Leaf)  (* base case *)
    | Node (_, v', l, r) ->
      begin
        match () with
          | _ when v < v' ->   (* insert into left subtree *)
            let l' = insert v l in  (* new left subtree *)
              if depth l' - depth r = 2  (* tree is now unbalanced *)
                then
                  if v < data l'
                    then  (* left-left case *)
                      right_rotate (make_node v' l' r)
                    else  (* left-right case *)
                      right_rotate (make_node v' (left_rotate l') r)
                else
                  make_node v' l' r  (* already balanced *)
          | _ when v > v' ->   (* insert into right subtree *)
              let r' = insert v r in  (* new left subtree *)
              if depth r' - depth l = 2  (* tree is now unbalanced *)
                then
                  if v < data r'
                    then  (* right-left case *)
                      left_rotate (make_node v' l (right_rotate r'))
                    else  (* right-right case *)
                      left_rotate (make_node v' l r')
                else
                  make_node v' l r'  (* already balanced *)
          | _ -> t  (* already in tree *)
      end
    
(* Find the minimum value in an AVL tree. *)
let rec min_avl_tree = function
  | Leaf -> None
  | Node (_, v, l, _) ->
    begin
      match min_avl_tree l with
        | None -> Some v
        | Some l' -> Some l'
    end

(* Find the maximum value in an AVL tree. *)
let rec max_avl_tree = function
  | Leaf -> None
  | Node (_, v, _, r) ->
    begin
      match max_avl_tree r with
        | None -> Some v
        | Some r' -> Some r'
    end

(* Return true if a tree is a valid AVL tree. *)
let rec is_avl_tree = function
  | Leaf -> true
  | Node (d, v, l, r) ->
    let dl = depth l in
    let dr = depth r in
    if is_avl_tree l
      && is_avl_tree r
      && d = 1 + max dl dr
      && abs (dl - dr) < 2
      then  (* check order invariant *)
        match (max_avl_tree l, min_avl_tree r) with
          | (None, None) -> true
          | (None, Some rmin) -> v <= rmin
          | (Some lmax, None) -> v >= lmax
          | (Some lmax, Some rmin) -> v >= lmax && v <= rmin
      else false