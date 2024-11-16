(* example.ml *)

(* Open the modules where the types and functions are defined *)
open Nonogram  (* Assuming types are defined in nonogram.ml *)
open Utils     (* Assuming utils.ml contains the print_nonogram function *)
open Brute_force

(* Define a sample Nonogram puzzle *)

(* Sample constraints for rows and columns *)
let sample_puzzle = {
  row_constraints = [
    [2];
    [2];
    [3];
    [3];
    [3];
  ];
  col_constraints = [
    [1; 1];
    [2; 2];
    [4];
    [2];
    [1];
  ];
}

let () =
  (* Solve the puzzle *)
  let solution_grid = solve_nonogram sample_puzzle in

  (* Create the nonogram structure *)
  let sample_nonogram = {
    puzzle = sample_puzzle;
    grid = solution_grid;
  } in

  (* Print the solution *)
  print_nonogram sample_nonogram