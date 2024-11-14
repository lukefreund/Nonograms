open Nonogram
open Utils
open Brute_force
 
 (* Example usage *)
 let () =


 (* Define the Nonogram puzzle *)
 let example_puzzle = {
  row_constraints = [
    [3];
    [2; 1];
    [3; 2];
    [2; 2];
    [6];
    [1; 5];
    [6];
    [1];
    [2]
  ];
  col_constraints = [
    [1; 2];
    [3; 1];
    [1; 5];
    [7; 1];
    [5];
    [3];
    [4];
    [3]
  ]
} in

(* Solve the puzzle *)
let solved_nonogram = solve_nonogram_brute_force example_puzzle in 
print_nonogram solved_nonogram