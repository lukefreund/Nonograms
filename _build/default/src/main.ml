(* example.ml *)

(* Open the modules where the types and functions are defined *)
open Nonogram  (* Assuming types are defined in nonogram.ml *)
open Utils     (* Assuming utils.ml contains the print_nonogram function *)

(* Define a sample Nonogram puzzle *)

(* Sample constraints for rows and columns *)
let sample_puzzle = {
  row_constraints = [
    [1];
    [3];
    [4];
    [3];
    [2];
  ];
  col_constraints = [
    [1];
    [1; 1];
    [3];
    [3];
    [4];
  ];
}

(* Sample grid matching the constraints *)
let sample_grid = [
  [Empty; Empty; Empty; Empty; Filled];
  [Empty; Empty; Filled; Filled; Filled];
  [Empty; Filled; Filled; Filled; Filled];
  [Empty; Empty; Filled; Filled; Filled];
  [Filled; Filled; Empty; Empty; Empty];
]

(* Combine the puzzle and grid into a nonogram *)
let sample_nonogram = {
  puzzle = sample_puzzle;
  grid = sample_grid;
}

(* Now, use the print_nonogram function to display it *)
let () =
  print_nonogram sample_nonogram
