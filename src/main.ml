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
 
 (* Assume we have a solved grid *)
 let example_grid = [
   [Empty; Filled; Filled; Filled; Empty; Empty; Empty; Empty];
   [Filled; Filled; Empty; Filled; Empty; Empty; Empty; Empty];
   [Empty; Filled; Filled; Filled; Empty; Empty; Filled; Filled];
   [Empty; Empty; Filled; Filled; Empty; Empty; Filled; Filled];
   [Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled];
   [Filled; Empty; Filled; Filled; Filled; Filled; Filled; Empty];
   [Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty];
   [Empty; Empty; Empty; Empty; Filled; Empty; Empty; Empty];
   [Empty; Empty; Empty; Filled; Filled; Empty; Empty; Empty];
 ] in
 
 (* Create the nonogram instance *)
 let example_nonogram = {
   puzzle = example_puzzle;
   grid = example_grid;
 } in
 
 (* Print the nonogram *)
 print_nonogram example_nonogram