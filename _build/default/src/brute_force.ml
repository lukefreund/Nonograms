open Nonogram
(* Helper function to generate all possible sequences for a given constraint and row length *)
let rec generate_sequences constraint_list length =
  let rec place_blocks blocks remaining_length =
    match blocks with
    | [] -> 
        if remaining_length = 0 then [[]]
        else [List.init remaining_length (fun _ -> Empty)]
    | block :: rest_blocks ->
        let max_start = remaining_length - (List.fold_left (+) 0 (block :: rest_blocks)) - List.length rest_blocks + 1 in
        let rec try_positions start =
          if start > max_start then []
          else
            let prefix = List.init start (fun _ -> Empty) in
            let block_cells = List.init block (fun _ -> Filled) in
            let suffix_options = place_blocks rest_blocks (remaining_length - start - block - 1) in
            let sequences = List.map (fun suffix -> prefix @ block_cells @ [Empty] @ suffix) suffix_options in
            sequences @ try_positions (start + 1)
        in
        try_positions 0
  in
  let sequences = place_blocks constraint_list length in
  List.filter (fun seq -> List.length seq = length) sequences

(* Function to transpose the grid *)
let transpose grid =
  let rec transpose_aux grid acc =
    match grid with
    | [] | [] :: _ -> List.rev acc
    | _ ->
        let column = List.map List.hd grid in
        let rest = List.map List.tl grid in
        transpose_aux rest (column :: acc)
  in
  transpose_aux grid []

(* Function to check if a sequence matches a given constraint *)
let sequence_matches_constraint sequence constraint_list =
  let rec extract_blocks seq =
    match seq with
    | [] -> []
    | Empty :: rest -> extract_blocks rest
    | Filled :: rest ->
        let rec count_filled cells count =
          match cells with
          | Filled :: tail -> count_filled tail (count + 1)
          | _ -> (count, cells)
        in
        let (block_length, remaining_seq) = count_filled rest 1 in
        block_length :: extract_blocks remaining_seq
  in
  extract_blocks sequence = constraint_list

(* Function to check if all columns in the grid satisfy the column constraints *)
let check_columns grid col_constraints =
  let transposed_grid = transpose grid in
  List.for_all2 sequence_matches_constraint transposed_grid col_constraints

(* Main brute-force solver function *)
let solve_nonogram_brute_force puzzle =
  let row_constraints = puzzle.row_constraints in
  let col_constraints = puzzle.col_constraints in
  let num_rows = List.length row_constraints in
  let num_cols = List.length col_constraints in

  (* Generate all possible sequences for each row *)
  let row_options = List.map (fun constraint_list ->
    generate_sequences constraint_list num_cols
  ) row_constraints in

  (* Generate all combinations of row sequences *)
  let rec generate_combinations options_list =
    match options_list with
    | [] -> [[]]
    | options :: rest ->
        let rest_combinations = generate_combinations rest in
        List.concat (List.map (fun option ->
          List.map (fun combination -> option :: combination) rest_combinations
        ) options)
  in

  let all_possible_grids = generate_combinations row_options in

  (* Try each grid and check if it satisfies the column constraints *)
  let rec try_grids grids =
    match grids with
    | [] -> None  (* No solution found *)
    | grid_rows :: rest ->
        if check_columns grid_rows col_constraints then Some grid_rows
        else try_grids rest
  in

  try_grids all_possible_grids