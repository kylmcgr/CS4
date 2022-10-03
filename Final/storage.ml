(* Student name: Kyle McGraw *)
(* Email: kmcgraw@caltech.edu *)

module Loc =
  struct
    type t = int * int

    let compare = Stdlib.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = 
      if nrows < 1 || ncols < 1
        then invalid_arg 
      (Printf.sprintf "make: invalid arguments: nrows = %i, ncols = %i"
      nrows ncols)
      else Array.make_matrix nrows ncols (-1)

    let get data (row, col) = 
      try
        if data.(row).(col) < 0
          then None
        else Some data.(row).(col)
      with Invalid_argument s -> None

    let set data (row, col) i = 
      try
        if i < 0
          then invalid_arg "set: negative argument"
        else data.(row).(col) <- i;
        data
      with Invalid_argument s -> invalid_arg
      (Printf.sprintf "set: invalid location: (%i, %i)"
      row col)

    let has_loc data (row, col) =
      if row < 0 || col < 0 || row >= (Array.length data.(0)) 
        || col >= (Array.length data)
        then false
      else data.(row).(col) <> -1

    let remove data (row, col) =
      try
        data.(row).(col) <- -1;
        data
      with Invalid_argument s -> data
  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)
module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols = 
      if nrows < 1 || ncols < 1
        then invalid_arg 
      (Printf.sprintf "make: invalid arguments: nrows = %i, ncols = %i"
      nrows ncols)
      else { contents = LocMap.empty; nrows; ncols }
    
    let get data (row, col) = 
      try
        Some (LocMap.find (row, col) data.contents)
      with Not_found -> None

    let set data (row, col) i = 
      if row < 0 || col < 0 || row >= data.nrows
        || col >= data.ncols
      then invalid_arg
      (Printf.sprintf "set: invalid location: (%i, %i)"
      row col)
      else if i < 0
        then invalid_arg "set: negative argument"
      else { data with contents = LocMap.update 
      (row, col) (fun f -> Some i) data.contents }

    let has_loc data (row, col) = 
      try
        (LocMap.find (row, col) data.contents) <> -1
      with Not_found -> false

    let remove data (row, col) =
      if row < 0 || col < 0 || row >= data.nrows
        || col >= data.ncols
        then data
      else { data with contents = (LocMap.remove 
      (row, col) data.contents) }
  end

