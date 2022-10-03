(* Student name: Kyle McGraw *)
(* Email: kmcgraw@caltech.edu *)

open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc

    let init_reachable nrows ncols =
      let moves = [(-2, -1); (-1, -2); (1, -2); 
      (2, -1); (2, 1); (1, 2); (-1, 2); (-2, 1)]
      in let locs = List.concat (List.init nrows 
      (fun a -> List.init ncols (fun b -> (a,b))))
      in let counts (r, c) = List.fold_left 
      (fun s (mr, mc) -> 
        if ok_loc nrows ncols (r+mr, c+mc)
          then s + 1 
        else s) 0 moves
      in List.fold_left (fun s l -> S.set s l (counts l)) 
      (S.make nrows ncols) locs

    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    let get_loc_counts_from_loc board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_loc_counts_from_loc" "location off board" loc
      else
        let moves = [(-2, -1); (-1, -2); (1, -2); 
        (2, -1); (2, 1); (1, 2); (-1, 2); (-2, 1)]
        in let unoccupied_onboard l = 
          if check_bounds board l
            then match get_index board l with
            None when check_bounds board l -> 
              get_reachable board l
            | _ -> None
          else None
        in let rec iter = function
        | [] -> []
        | (mr, mc) :: t ->
          let (r, c) = loc in 
          let new_loc = (r+mr, c+mc) in 
          match unoccupied_onboard new_loc with
          None -> iter t
          | Some c -> (new_loc, c) :: (iter t)
        in iter moves
    
    let place board loc = 
      if not (check_bounds board loc) then
        bad_loc "place" "location off board" loc
      else match get_index board loc with
      Some _ -> bad_loc "place" "location occupied" loc
      | None -> 
        let moves = [(-2, -1); (-1, -2); (1, -2); 
        (2, -1); (2, 1); (1, 2); (-1, 2); (-2, 1)]
        in let (r, c) = loc in 
        let not_move_away (mr, mc) = 
          if check_bounds board (r+mr, c+mc) 
            then match get_index board (r+mr, c+mc) with
            None -> true
            | _ -> false
          else true
        in if List.length board.placed > 0 
          && List.for_all not_move_away moves
          then bad_loc "place" "loc is not a knight's move" loc
        else
          let placed = loc :: board.placed in
          let last_index = board.last_index + 1 in
          let indices = S.set board.indices loc last_index in
          let update reaches (mr, mc) = 
            let new_loc = (r+mr, c+mc) in 
            if check_bounds board new_loc 
              then match get_reachable board new_loc with
              None -> reaches
              | Some s -> S.set reaches new_loc (s-1)
            else reaches
          in let reachable = List.fold_left update 
          (S.remove board.reachable loc) moves in
          { board with placed; last_index; indices; reachable }

    let undo board = 
      if board.last_index = 0
        then board
      else
        let moves = [(-2, -1); (-1, -2); (1, -2); 
        (2, -1); (2, 1); (1, 2); (-1, 2); (-2, 1)]
        in let (r, c) = List.hd board.placed in
        let placed = List.tl board.placed in
        let last_index = board.last_index - 1 in
        let indices = S.remove board.indices (r, c) in
        let new_val = List.fold_left 
        (fun s (mr, mc) -> 
          if check_bounds board (r+mr, c+mc)
            then match get_reachable board (r+mr, c+mc) with
            None -> s
            | Some a -> s+1
          else s) 0 moves
        in let update reaches (mr, mc) = 
          let new_loc = (r+mr, c+mc) in 
          if check_bounds board new_loc 
            then match get_reachable board new_loc with
            None -> reaches
            | Some s -> S.set reaches new_loc (s+1)
          else reaches
        in let reachable = List.fold_left update 
        (S.set board.reachable (r, c) new_val) moves in
        { board with placed; last_index; indices; reachable }

    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end