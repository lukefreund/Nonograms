(* main.ml *)

open Nonogram
open Utils
open Solver

let () =
  Random.self_init ();  (* Initialize the random number generator *)

  (* Greet the user and explain the game *)
  print_endline "Welcome to the Nonogram Game!";
  print_endline "You will be presented with a Nonogram puzzle to solve.";
  print_endline "Please enter the size of the Nonogram puzzle you want to play (e.g., 5):";

  (* Read the size from user input *)
  let size =
    try
      let line = read_line () in
      let n = int_of_string line in
      if n <= 0 then (
        print_endline "Size must be a positive integer. Using default size 5.";
        5
      ) else n
    with
    | Failure _ ->
      print_endline "Invalid input. Using default size 5.";
      5
  in

  (* Generate a random Nonogram puzzle *)
  let random_nonogram = generate_random_nonogram size in

  (* Function to print constraints nicely *)
  let print_constraints puzzle =
    print_endline "Row Constraints:";
    List.iteri (fun i constraints ->
      Printf.printf "Row %d: %s\n" (i + 1) (if constraints = [] then "0" else String.concat ", " (List.map string_of_int constraints))
    ) puzzle.row_constraints;
    print_endline "\nColumn Constraints:";
    List.iteri (fun i constraints ->
      Printf.printf "Column %d: %s\n" (i + 1) (if constraints = [] then "0" else String.concat ", " (List.map string_of_int constraints))
    ) puzzle.col_constraints
  in

  (* Print the puzzle constraints *)
  print_endline "\nHere are the puzzle constraints:\n";
  print_constraints random_nonogram.puzzle;

  (* Ask the user if they are ready for the answer *)
  print_endline "\nAre you ready for the answer? (Press Enter to continue)";
  let _ = read_line () in

  (* Solve the puzzle *)
  print_endline "\nSolving the puzzle...";
  let solution_grid = solve_nonogram_eff random_nonogram.puzzle in

  (* Create the solved nonogram structure *)
  let solved_nonogram = {
    puzzle = random_nonogram.puzzle;
    grid = solution_grid;
  } in

  (* Print the solution *)
  print_endline "\nHere is the solution:\n";
  print_nonogram solved_nonogram
