open Nonogram

(* Define list_init if List.init is not available *)
  let list_init n f =
    let rec aux i acc =
      if i < 0 then acc
      else aux (i - 1) (f i :: acc)
    in
    aux (n - 1) []
  
  (* Function to generate all possible sequences for a given constraint and length *)
  let generate_sequences constraint_list length =
    let rec place_blocks blocks remaining_length =
      match blocks with
      | [] ->
          if remaining_length >= 0 then
            [list_init remaining_length (fun _ -> Empty)]
          else []
      | block :: rest_blocks ->
          let min_space = if rest_blocks = [] then 0 else 1 in
          let max_start = remaining_length - (List.fold_left (+) 0 (block :: rest_blocks)) - (List.length rest_blocks) + 1 in
          let rec try_positions start acc =
            if start > max_start then acc
            else
              let prefix = list_init start (fun _ -> Empty) in
              let block_cells = list_init block (fun _ -> Filled) in
              let suffix_options = place_blocks rest_blocks (remaining_length - start - block - min_space) in
              let sequences = List.map (fun suffix ->
                prefix @ block_cells @ (if rest_blocks = [] then [] else [Empty]) @ suffix
              ) suffix_options in
              try_positions (start + 1) (sequences @ acc)
          in
          try_positions 0 []
    in
    let sequences = place_blocks constraint_list length in
    (* Filter sequences to correct length *)
    List.filter (fun seq -> List.length seq = length) sequences
  
  (* Function to transpose a grid *)
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
          let grid = List.rev grid_rows in  (* Since we built rows in reverse order *)
          if check_columns grid col_constraints then Some {puzzle = puzzle; grid = grid}
          else try_grids rest
    in
  
    try_grids all_possible_grids