(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:                *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b = 
  let check_piece = fun c p -> LocSet.equal p 
  (LocSet.of_list [(3,1); (4,1); (3,2); (4,2)])
  in CharMap.exists check_piece b.pieces

let compare b1 b2 = 
  if LocSet.equal b1.unoccupied b2.unoccupied then
    let rec iter = function
    | [] -> []
    | (c, locs) :: t -> locs :: (iter t)
    in
    LocSetSet.compare 
    (LocSetSet.of_list (iter (CharMap.bindings b1.pieces)))
    (LocSetSet.of_list (iter (CharMap.bindings b2.pieces)))
  else LocSet.compare b1.unoccupied b2.unoccupied

let remove c ({ pieces = p; unoccupied = u } as b) = 
  if CharMap.mem c p
    then { pieces = CharMap.remove c p; unoccupied = LocSet.union u (CharMap.find c p)}
  else b

let add (c, p) { pieces = ps; unoccupied = u } = 
  if LocSet.subset p u && not (CharMap.mem c ps)
    then Some { pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p }
  else None

let make_move (c, d, i) b =
  if CharMap.mem c b.pieces && i>0
    then let pieces = CharMap.find c b.pieces
    in let do_move n d (r, c) = 
      match d with
      Up -> (r-n, c)
      | Down -> (r+n, c)
      | Left -> (r, c-n)
      | Right -> (r, c+n)
    in let rec can_move n d (r, c) = 
      let (r1, c1) = do_move n d (r, c) in
      if n = 0
        then true
      else (LocSet.mem (r1, c1) (LocSet.union pieces b.unoccupied))
      && r1 >= 0 && r1 <= 4 && c1 >= 0 && c1 <= 3
      && can_move (n-1) d (r, c)
    in let locs = LocSet.map (do_move i d) pieces
    in if LocSet.for_all (can_move i d) pieces
        then add (c, locs) (remove c b)
      else None
  else None
  
let next b =
  let rec moves c i d = 
    match make_move (c, d, i) b with
    None -> []
    | Some b1 -> b1 :: moves c (i+1) d
  in let map_func (c, locs) =
    List.concat_map (moves c 1) [Up; Down; Left; Right]
  in List.concat_map map_func (CharMap.bindings b.pieces)

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

