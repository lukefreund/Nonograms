(* brute_force.ml *)

open Nonogram

(* Generates all sequences of cells for a row that satisfy the given constraints *)
let generate_row_possibilities constraints row_length =
  let rec aux constraints remaining_length =
    match constraints with
    | [] ->
      if remaining_length >= 0 then [ List.init remaining_length (fun _ -> Empty) ]
      else []
    | n :: rest_constraints ->
      let min_spaces = if rest_constraints = [] then 0 else 1 in
      let sum_rest_constraints = sum_constraints rest_constraints in
      let max_prefix_spaces = remaining_length - (n + min_spaces + sum_rest_constraints) in
      if max_prefix_spaces < 0 then []
      else
        let possible_prefix_spaces = List.init (max_prefix_spaces + 1) (fun i -> i) in
        List.concat (List.map (fun spaces ->
          let prefix = List.init spaces (fun _ -> Empty) in
          let block = List.init n (fun _ -> Filled) in
          let suffix_length = remaining_length - spaces - n - min_spaces in
          let suffix_possibilities = aux rest_constraints suffix_length in
          List.map (fun suffix ->
            prefix @ block @ (if rest_constraints = [] then [] else [Empty]) @ suffix
          ) suffix_possibilities
        ) possible_prefix_spaces)
  and sum_constraints constraints =
    match constraints with
    | [] -> 0
    | _ -> List.fold_left ( + ) 0 constraints + (List.length constraints - 1)
  in
  List.filter (fun row -> List.length row = row_length) (aux constraints row_length)

(* Extracts columns from the grid *)
let columns_of_grid grid =
  let rec transpose grid =
    match grid with
    | [] | ([] :: _) -> []
    | _ -> List.map List.hd grid :: transpose (List.map List.tl grid)
  in
  transpose grid

(* Computes the constraints of a line (row or column) *)
let line_constraints line =
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

(* Checks if the grid's columns satisfy the column constraints *)
let check_columns grid col_constraints =
  let cols = columns_of_grid grid in
  List.for_all2 (fun col constr ->
    line_constraints col = constr
  ) cols col_constraints

(* Generates all combinations of row possibilities *)
let rec all_combinations lists =
  match lists with
  | [] -> [ [] ]
  | l :: ls ->
    let rest_combinations = all_combinations ls in
    List.concat (List.map (fun x ->
      List.map (fun rest -> x :: rest) rest_combinations
    ) l)

(* Main solver function *)
let solve_nonogram (puzzle : puzzle) : grid =
  let row_constraints = puzzle.row_constraints in
  let col_constraints = puzzle.col_constraints in
  let width = List.length col_constraints in

  let start_row_time = Unix.gettimeofday () in

  (* Generate all possible rows *)
  let all_row_possibilities = 
    List.map (fun constraints ->
      generate_row_possibilities constraints width
    ) row_constraints
  in
  let end_row_time = Unix.gettimeofday () in
  let row_generation_time = end_row_time -. start_row_time in
  Printf.printf "Time to generate all row possibilities: %.6f seconds\n" row_generation_time;

  let start_grid_time = Unix.gettimeofday () in
  (* Generate all possible grids *)
  let possible_grids = all_combinations all_row_possibilities in
  let end_grid_time = Unix.gettimeofday () in
  let grid_generation_time = end_grid_time -. start_grid_time in
  Printf.printf "Time to generate all possible grids: %.6f seconds\n" grid_generation_time;

  let start_check_time = Unix.gettimeofday () in
  (* Find the first grid that satisfies the column constraints *)
  let solution =
    try
      List.find (fun grid ->
        check_columns grid col_constraints
      ) possible_grids
    with Not_found ->
      failwith "No solution exists for the given puzzle."
  in
  let end_check_time = Unix.gettimeofday () in
  let column_check_time = end_check_time -. start_check_time in
  Printf.printf "Time to check grids against column constraints: %.6f seconds\n" column_check_time;

  (* Total time *)
  let total_time = row_generation_time +. grid_generation_time +. column_check_time in
  Printf.printf "Total time in solve_nonogram: %.6f seconds\n" total_time;

  solution
