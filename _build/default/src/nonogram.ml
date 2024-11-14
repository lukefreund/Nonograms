(* nonogram.ml *)

(* Type representing a cell in the Nonogram grid *)
type cell = 
  | Filled    (* The cell is filled *)
  | Empty     (* The cell is empty *)

(* Type representing the grid, which is a 2D list of cells *)
type grid = cell list list

(* Type representing the Nonogram puzzle constraints *)
type puzzle = {
  row_constraints : int list list;  (* Constraints for each row *)
  col_constraints : int list list;  (* Constraints for each column *)
}

(* Type representing the entire Nonogram problem, including the puzzle and grid *)
type nonogram = {
  puzzle : puzzle;  (* The constraints of the puzzle *)
  grid : grid;      (* The current state of the grid *)
}

