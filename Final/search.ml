(* Student name: Kyle McGraw *)
(* Email: kmcgraw@caltech.edu *)

open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions go here. *)

    let search nrows ncols start_row start_col print =
      let b = B.make nrows ncols in
      let b2 = B.place b (start_row, start_col) in
      let rec iter b = 
        if B.is_solved b
          then begin if print
            then P.print_board b false;
              Some (B.get_placed b)
            end
        else
          let last = B.get_last b in
          let loccounts = B.get_loc_counts_from_loc b last in
          if loccounts = []
            then None
          else
            let rec mins lst min = function
            | [] -> lst
            | (loc, c) :: t when c < min -> mins [loc] c t
            | (loc, c) :: t when c = min -> mins (loc :: lst) c t
            | _ :: t -> mins lst min t
            in let min_locs = mins [] 8 loccounts in
            let i = Random.int (List.length min_locs) in
            let loc = List.nth min_locs i in
            iter (B.place b loc)
      in iter b2

  end
  
  
