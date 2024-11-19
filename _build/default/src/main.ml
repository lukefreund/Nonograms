open Grid
open Solver
open Constraints

let () =
  let rows = ["1"; "1"; "1"] in
  let cols = ["1"; "1"; "1"] in
  let row_constraints, col_constraints = parse_constraints rows cols in
  let grid = create_empty_grid 3 3 in
  match solve_nonogram grid (row_constraints, col_constraints) with
  | Some solution ->
      print_endline "Solution found:";
      print_grid solution
  | None ->
      print_endline "No solution exists."