(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let search init = 
      let storage = S.create () in
      let new_history = [init] in
      S.push new_history storage;
      let prev_boards = DS.empty in
      let rec iter prev_boards =
        if S.is_empty storage
          then raise Not_found
        else
          let next_history = S.pop storage in
          let last_board = List.hd next_history in
          if DS.mem last_board prev_boards
            then iter prev_boards
          else if D.is_solved last_board
            then next_history
          else
            begin
            List.iter (fun b -> S.push (b :: next_history) storage) 
            (D.next last_board);
            iter (DS.add last_board prev_boards)
            end
      in iter prev_boards

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

