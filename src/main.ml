(* example.ml *)

(* Open the modules where the types and functions are defined *)
open Nonogram  (* Assuming types are defined in nonogram.ml *)
open Utils     (* Assuming utils.ml contains the print_nonogram function *)
open Solver


(* Define a sample Nonogram puzzle *)

(* Sample constraints for rows and columns *)
let sample_puzzle = {
  row_constraints = [
    [5; 1; 1];
    [2; 5];
    [3; 1];
    [3; 4];
    [4; 2];
    [4; 2];
    [1; 1; 1; 3];
    [4];
    [2];
    [1];
  ];
  col_constraints = [
    [3; 2];
    [6];
    [1; 5];
    [1; 2];
    [1; 1; 2];
    [2; 1];
    [1; 1; 4];
    [2; 1; 4];
    [1; 4];
    [5];
  ];
}


let () =
  (* Solve the puzzle *)
  let solution_grid = solve_nonogram_eff sample_puzzle in

  (* Create the nonogram structure *)
  let sample_nonogram = {
    puzzle = sample_puzzle;
    grid = solution_grid;
  } in

  (* Print the solution *)
  print_nonogram sample_nonogram