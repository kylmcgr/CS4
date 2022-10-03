(* A.1 *)
(*

  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]

  FUNCTION 0 (fun n -> let rec iter m r = ...)
    env: FRAME 0
    param: n
    body: let rec iter m r = ...

  FRAME 1 (let factorial = FUNCTION 0 in factorial 3)
    parent: FRAME 0
    bindings:
      factorial : FUNCTION 0

  FRAME 2 (FUNCTION 0 applied to 3)
    parent: FRAME 1
    bindings:
      n : 3

  FRAME 3  (let rec iter m r = FUNCTION 1 in iter n 1)
    parent: FRAME 2
    bindings:
      iter : FUNCTION 1

  FUNCTION 1 (fun m r -> if m = 0 ...)
    env: FRAME 3
    param: m, r
    body: if m = 0 ...
      
  FRAME 4 (FUNCTION 1 applied to 3, 1)
    parent: FRAME 3
    bindings: 
      m : 3
      r : 1
      
  FRAME 5 (FUNCTION 1 applied to 2, 3)
    parent: FRAME 3
    bindings:
      m : 2
      r : 3
      
  FRAME 6 (FUNCTION 1 applied to 1, 6)
    parent: FRAME 3
    bindings:
      m : 1
      r : 6
      
  FRAME 7 (FUNCTION 1 applied to 0, 6)
    parent: FRAME 3
    bindings:
      m : 0
      r : 6
*)

(* A.2 *)
let factorial =
  let f = ref (fun n -> 0) in
  begin
    f := fun n ->
    match n with
      | 0 -> 1
      | _ -> n * (!f (n-1));
  end;
  !f

(* B.1 *)
exception Stat_error of string

let make_stat_1 () = 
  let sum = ref 0. in
  let sumsq = ref 0. in
  let n = ref 0. in
  object
  method append a = 
    sum := !sum +. a;
    sumsq := !sumsq +. (a *. a);
    n := !n +. 1.
  method clear = 
    sum := 0.;
    sumsq := 0.;
    n := 0.
  method mean = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for mean")
    else !sum /. !n
  method variance = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for variance")
    else (!sumsq -. (!sum *. !sum /. !n)) /. !n
  method stdev = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for stdev")
    else sqrt ((!sumsq -. (!sum *. !sum /. !n)) /. !n)
  end

(* B.2 *)
let make_stat_2 () = 
  let sum = ref 0. in
  let sumsq = ref 0. in
  let n = ref 0. in
  let _variance n = 
    (!sumsq -. (!sum *. !sum /. !n)) /. !n
  in
  object
  method append a = 
    sum := !sum +. a;
    sumsq := !sumsq +. (a *. a);
    n := !n +. 1.
  method clear = 
    sum := 0.;
    sumsq := 0.;
    n := 0.
  method mean = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for mean")
    else !sum /. !n
  method variance = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for variance")
    else _variance n
  method stdev = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for stdev")
    else sqrt (_variance n)
  end
  
(* C.1 *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf
    let is_empty = function
    | Leaf -> true
    | _ -> false
    let find_min = function
    | Leaf -> raise Empty
    | Node (_,a,_,_) -> a
    let find_rank = function
    | Leaf -> 0
    | Node (v,_,_,_) -> v
    let make_new a q1 q2 =
			let v1 = find_rank q1 in
			let v2 = find_rank q2 in
				if v1 < v2 then
					Node (v1+1,a,q2,q1)
				else
					Node (v2+1,a,q1,q2)
		let rec merge q1 q2 =
			match (q1, q2) with
				| (Leaf, q) -> q
				| (q, Leaf) -> q
				| (Node (_,a,l,r), q) when a < find_min q ->
					make_new a l (merge r q)
				| (q, Node (_,a,l,r)) ->
					make_new a l (merge r q)
    let insert q a = merge q (Node (1,a,Leaf,Leaf))
    let delete_min = function
    | Leaf -> raise Empty
    | Node (_,_,l,r) -> merge l r 
    let rec from_list = function
    | [] -> Leaf
    | h :: t -> insert (from_list t) h
  end

let heap_sort l = 
  let rec iter q l2 =
    if PriorityQueue.is_empty q then
      List.rev l2
    else iter (PriorityQueue.delete_min q) 
    (PriorityQueue.find_min q :: l2)
  in iter (PriorityQueue.from_list l) []
  
(* C.2 *)
(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
  sig
    type t
    val cmp: t -> t -> comparison
  end

module MakePriorityQueue (Elt : ORDERED)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty

    type elem = Elt.t

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf
    let is_empty = function
    | Leaf -> true
    | _ -> false
    let find_min = function
    | Leaf -> raise Empty
    | Node (_,a,_,_) -> a
    let find_rank = function
    | Leaf -> 0
    | Node (v,_,_,_) -> v
    let make_new a q1 q2 =
			let v1 = find_rank q1 in
			let v2 = find_rank q2 in
				if v1 < v2 then
					Node (v1+1,a,q2,q1)
				else
					Node (v2+1,a,q1,q2)
		let rec merge q1 q2 =
			match (q1, q2) with
				| (Leaf, q) -> q
				| (q, Leaf) -> q
				| (Node (_,a,l,r), q) when Elt.cmp a (find_min q) = LT ->
					make_new a l (merge r q)
				| (q, Node (_,a,l,r)) ->
					make_new a l (merge r q)
    let insert q a = merge q (Node (1,a,Leaf,Leaf))
    let delete_min = function
    | Leaf -> raise Empty
    | Node (_,_,l,r) -> merge l r 
    let rec from_list = function
    | [] -> Leaf
    | h :: t -> insert (from_list t) h
  end

module OrderedString =
  struct
    type t = string
    let cmp x y =
      if x = y then EQ else if x < y then LT else GT
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 l = 
  let rec iter q l2 =
    if StringPQ.is_empty q then
      List.rev l2
    else iter (StringPQ.delete_min q) 
    (StringPQ.find_min q :: l2)
  in iter (StringPQ.from_list l) []
  
(* D.1 *)
type 'a contents = 
| Eval of 'a
| Uneval of (unit -> 'a)
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Uneval e)
let force lz = match !lz with
| Eval a -> a
| Uneval a -> begin
  lz := Eval (a ());
  a ()
end

(* D.2 *)
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

(* D.2.a *)
let almost_sum =
  fun g ->
    let f = ref (fun n -> 0) in
    begin
      f := fun n ->
      match n with
        | [] -> 0
        | h :: t -> h + !f t;
    end;
    !f

let sum = y almost_sum

(* D.2.b *)
let almost_factorial =
  fun g ->
    let f = ref (fun (n, r) -> 0) in
    begin
      f := fun (n, r) ->
      match (n, r) with
        | (0, r) -> r
        | (n, r) -> !f (n-1, r*n);
    end;
    !f

let factorial2 n = y almost_factorial (n, 1)