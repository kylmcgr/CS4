(* Board locations: row, column coordinates, starting from 0. *)
type loc = int * int

(* Directions of moves on the board. *)
type dir = Up | Down | Left | Right

(* Moves on the board. *)
type move = char * dir * int

(* Sets of locations. *)
module LocSet : Set.S with type elt = loc

(* A map between characters and sets of locations. *)
module CharMap : Map.S with type key = char

(* The board representation.  A board is a record of:
 *
 * a) pieces: a map between characters (the piece labels) and
 *    the set of locations occupied by the piece
 *
 * b) a set of unoccupied locations
 *
 * Obviously, the sets of locations must be disjoint (no overlap).
 *)
type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

val read : string -> t
val show : t -> string

(* Return true if a board is solved. *)
val is_solved : t -> bool

(* Compare two boards to see which is "bigger", "smaller" or if
 * they are "equal".
 * We call two boards "equal" when the same piece sets
 * exist on two boards and the unoccupied sets are the same
 * on both boards. Piece labels are irrelevant. *)
val compare : t -> t -> int

(* Remove a piece from a board.  Update the "unoccupied" set.
 * Return the resulting board. 
 * If the piece isn't on the board, return it unchanged. *)
val remove : CharMap.key -> t -> t

(* Place a piece on a board. Return the new board.
 * The piece is represented by a tuple of the character label
 * and the set of locations it will occupy.
 * Update the "unoccupied" set.
 * Return None if the piece is already on the board 
 * or if it can't be placed on the board because not all of its
 * locations are unoccupied. *)
val add : CharMap.key * LocSet.t -> t -> t option

(* Attempt to make a move (char * dir * int) on a board.
 * Return None if the move failed for any reason
 * (including if the labeled piece is not on the board).
 * Also return None if the move distance (int) is < 1.
 * Return Some (new board) if it succeeded. *)
val make_move : move -> t -> t option

(* Return a list of boards that can be obtained from the current
 * board by a single move. *)
val next : t -> t list

