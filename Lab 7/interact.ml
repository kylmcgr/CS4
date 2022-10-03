(* interact.ml: Playing Klotski interactively. *)

open Boards
open Klotski

(* Read a move from a 3-character string. *)
let read_move s =
  let char_to_int c =
    let j = int_of_char c - int_of_char '0' in
      if j < 1 || j > 4
        then None
        else Some j
  in
  let char_to_dir d =
    match d with
      | 'l' -> Some Left
      | 'r' -> Some Right
      | 'u' -> Some Up
      | 'd' -> Some Down
      | _ -> None
  in
  let check_char c =
    if c < 'a' || c > 'z'
      then None
      else Some c
  in
    if String.length s <> 3
      then None
      else
        let c = check_char s.[0] in
        let d = char_to_dir s.[1] in
        let i = char_to_int s.[2] in
          match (c, d, i) with
            | (Some c', Some d', Some i') -> Some (c', d', i')
            | _ -> None

let rec interact b =
  Printf.printf "\n%s\n" (show b);
  if is_solved b
    then Printf.printf "The board is solved!\n"
    else
      begin
        Printf.printf "Enter move: ";
        let line = read_line () in
          match read_move line with
            | None -> 
                begin
                  Printf.printf "\nERROR: invalid move.\n";
                  interact b
                end
            | Some move ->
                begin
                  match make_move move b with
                    | None -> 
                        begin
                          Printf.printf "Invalid move; try again.\n";
                          interact b
                        end
                    | Some b' -> interact b'
                end
      end

let _ =
  let args = Sys.argv in
    match args with
      | [| _; n |] -> 
          let n' = int_of_string n in
          let b = read boards.(n') in
            interact b
      | _ -> 
        let usagestr = Printf.sprintf "usage: %s n\n" args.(0) in
          begin
            print_string usagestr;
            print_string "    (n : an int between 0 and 7)\n"
          end

