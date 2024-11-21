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

(* Function to generate a random Nonogram puzzle *)
let generate_random_nonogram (l : int) : nonogram =
  let grid = Array.init l (fun _ ->
    Array.init l (fun _ ->
      if Random.bool () then Filled else Empty
    )
  ) in

  (* Function to compute constraints for a line *)
  let compute_constraints line =
    let rec aux cells acc current_count =
      match cells with
      | [] ->
        if current_count > 0 then List.rev (current_count :: acc)
        else List.rev acc
      | Filled :: rest ->
        aux rest acc (current_count + 1)
      | Empty :: rest ->
        if current_count > 0 then aux rest (current_count :: acc) 0
        else aux rest acc 0
    in
    aux line [] 0
  in

  (* Compute row constraints *)
  let row_constraints = Array.to_list (
    Array.map (fun row ->
      compute_constraints (Array.to_list row)
    ) grid
  ) in

  (* Compute column constraints *)
  let col_constraints =
    let transposed_grid = Array.init l (fun col ->
      Array.init l (fun row -> grid.(row).(col))
    ) in
    Array.to_list (
      Array.map (fun col ->
        compute_constraints (Array.to_list col)
      ) transposed_grid
    )
  in

  (* Convert grid from array to list *)
  let grid_list = Array.to_list (Array.map Array.to_list grid) in

  (* Create the puzzle and nonogram *)
  let puzzle = {
    row_constraints = row_constraints;
    col_constraints = col_constraints;
  } in

  let nonogram = {
    puzzle = puzzle;
    grid = grid_list;
  } in

  nonogram