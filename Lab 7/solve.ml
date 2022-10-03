(* solve.ml: Klotski board solver *)

open Boards
open Klotski
open Search

module DFS = Search(Stack)
module BFS = Search(Queue)
module K_DFS = DFS(Klotski)
module K_BFS = BFS(Klotski)

let search_and_print search show b =
  let h = search b in
    begin
      print_newline ();
      print_string (show h);
      Printf.printf "Solution in %d moves.\n\n" (List.length h - 1)
    end

let _ =
  let args = Sys.argv in
    match args with
      | [| _; "-dfs"; n |] ->
          let n' = int_of_string n in
          let b = read boards.(n') in
            search_and_print K_DFS.search K_DFS.show_history b
      | [| _; n |] -> 
          let n' = int_of_string n in
          let b = read boards.(n') in
            search_and_print K_BFS.search K_BFS.show_history b
      | _ -> 
        let usagestr = Printf.sprintf "usage: %s [-dfs] n\n" args.(0) in
          begin
            print_string usagestr;
            print_string "    (n : an int between 0 and 7)\n"
          end
