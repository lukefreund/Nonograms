(* Define the types for better readability *)
type cell = 
  | Empty     (* 0 *)
  | Filled    (* 1 *)
  | Blocked   (* -1 *)

type grid = cell array array

(* Function to initialize the grid with Empty cells *)
let initialize_grid rows cols : grid =
  Array.init rows (fun _ -> Array.init cols (fun _ -> Empty))

(* Function to get a row from the grid *)
let get_row (g : grid) row_idx : cell array =
  g.(row_idx)

(* Function to get a column from the grid *)
let get_col (g : grid) col_idx : cell array =
  Array.init (Array.length g) (fun row -> g.(row).(col_idx))

(* Function to extract blocks of consecutive Filled cells *)
let extract_blocks (line : cell array) : int list =
  let blocks = ref [] in
  let current = ref 0 in
  Array.iter (fun cell ->
    match cell with
    | Filled -> incr current
    | _ -> if !current > 0 then (blocks := !current :: !blocks; current := 0)
  ) line;
  if !current > 0 then blocks := !current :: !blocks;
  List.rev !blocks

(* Function to check partial satisfaction of hints *)
let satisfies_hint_partial (line : cell array) (hint : int list) : bool =
  let blocks = extract_blocks line in
  let rec aux blk h =
    match blk, h with
    | [], _ -> true  (* No blocks yet, still possible *)
    | blk, [] -> false (* More blocks than hints *)
    | b::bs, h::hs ->
        if b > h then false
        else if b = h then aux bs hs
        else
          (* b < h, still building this block *)
          true
  in
  aux blocks hint

(* Function to check full satisfaction of hints *)
let satisfies_hint_full (line : cell array) (hint : int list) : bool =
  let blocks = extract_blocks line in
  blocks = hint

(* Function to check if placing a value maintains consistency *)
let is_valid (g : grid) (row_hints : int list array) (col_hints : int list array) (row : int) (col : int) : bool =
  let row_ok = satisfies_hint_partial (get_row g row) row_hints.(row) in
  let col_ok = satisfies_hint_partial (get_col g col) col_hints.(col) in
  row_ok && col_ok

(* Function to check if the entire grid is solved *)
let is_solved (g : grid) (row_hints : int list array) (col_hints : int list array) : bool =
  let rows_ok = 
    Array.mapi (fun i row -> satisfies_hint_full row row_hints.(i)) g
    |> Array.for_all (fun x -> x)
  in
  let cols_ok =
    Array.mapi (fun j _ -> 
      let col = get_col g j in
      satisfies_hint_full col col_hints.(j)
    ) g.(0)
    |> Array.for_all (fun x -> x)
  in
  rows_ok && cols_ok

(* Function to count valid options for a cell *)
let count_valid_options (g : grid) (row_hints : int list array) (col_hints : int list array) (row : int) (col : int) : int =
  let original = g.(row).(col) in
  (* Try Filled *)
  g.(row).(col) <- Filled;
  let filled_valid = is_valid g row_hints col_hints row col in
  g.(row).(col) <- original;  (* Restore original state *)
  (* Try Blocked *)
  g.(row).(col) <- Blocked;
  let blocked_valid = is_valid g row_hints col_hints row col in
  g.(row).(col) <- original;  (* Restore original state *)
  let count = ref 0 in
  if filled_valid then incr count;
  if blocked_valid then incr count;
  !count

(* Function to find the next cell using MCV heuristic *)
let find_next_cell_mcv (g : grid) (row_hints : int list array) (col_hints : int list array) : (int * int) option =
  let rows = Array.length g in
  let cols = if rows = 0 then 0 else Array.length g.(0) in
  let min_options = ref 3 in  (* Since max options per cell are 2 *)
  let selected_cell = ref None in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      match g.(i).(j) with
      | Empty ->
          let options = count_valid_options g row_hints col_hints i j in
          if options < !min_options then (
            min_options := options;
            selected_cell := Some (i, j)
          )
      | _ -> ()
    done
  done;
  !selected_cell

(* Function to propagate constraints after filling/blocking a cell *)
let propagate_constraints (g : grid) (row_hints : int list array) (col_hints : int list array) : bool =
  let rows = Array.length g in
  let cols = if rows = 0 then 0 else Array.length g.(0) in
  let rec check_rows i =
    if i >= rows then true
    else
      let row = get_row g i in
      satisfies_hint_partial row row_hints.(i) && check_rows (i + 1)
  in
  let rec check_cols j =
    if j >= cols then true
    else
      let col = get_col g j in
      satisfies_hint_partial col col_hints.(j) && check_cols (j + 1)
  in
  check_rows 0 && check_cols 0

(* Function to finalize the grid by blocking remaining Empty cells *)
let finalize_grid (g : grid) : grid =
  Array.iteri (fun i row ->
    Array.iteri (fun j cell ->
      match cell with
      | Empty -> g.(i).(j) <- Blocked
      | _ -> ()
    ) row
  ) g;
  g

(* Recursive backtracking solver with MCV and Constraint Propagation *)
let rec solve_nonogram (g : grid) (row_hints : int list array) (col_hints : int list array) : bool * grid =
  if is_solved g row_hints col_hints then
    (true, g)
  else
    match find_next_cell_mcv g row_hints col_hints with
    | None -> (false, g)  (* No solution found *)
    | Some (row, col) ->
        (* Try Filled *)
        let () = g.(row).(col) <- Filled in
        if is_valid g row_hints col_hints row col && propagate_constraints g row_hints col_hints then
          let success, solution = solve_nonogram g row_hints col_hints in
          if success then
            (true, solution)
          else
            (* Backtrack and try Blocked *)
            let () = g.(row).(col) <- Blocked in
            if is_valid g row_hints col_hints row col && propagate_constraints g row_hints col_hints then
              let success, solution = solve_nonogram g row_hints col_hints in
              if success then
                (true, solution)
              else
                (* Backtrack *)
                let () = g.(row).(col) <- Empty in
                (false, g)
            else
              (* Backtrack *)
              let () = g.(row).(col) <- Empty in
              (false, g)
        else
          (* Backtrack and try Blocked *)
          let () = g.(row).(col) <- Blocked in
          if is_valid g row_hints col_hints row col && propagate_constraints g row_hints col_hints then
            let success, solution = solve_nonogram g row_hints col_hints in
            if success then
              (true, solution)
            else
              (* Backtrack *)
              let () = g.(row).(col) <- Empty in
              (false, g)
          else
            (* Backtrack *)
            let () = g.(row).(col) <- Empty in
            (false, g)

(* Function to print the grid *)
let print_grid (g : grid) : unit =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      match cell with
      | Filled -> print_string "█ "
      | Blocked -> print_string "░ "
      | Empty -> print_string ". "
    ) row;
    print_newline ()
  ) g

(* Function to validate hints against grid size *)
let validate_hints (row_hints : int list array) (col_hints : int list array) (rows : int) (cols : int) : bool =
  (* Simple validation: sum of hints + minimum required spaces should not exceed grid size *)
  let valid = ref true in
  Array.iter (fun hint ->
    let total_filled = List.fold_left (+) 0 hint in
    let min_required = total_filled + (List.length hint - 1) in
    if min_required > cols then valid := false
  ) row_hints;
  Array.iter (fun hint ->
    let total_filled = List.fold_left (+) 0 hint in
    let min_required = total_filled + (List.length hint - 1) in
    if min_required > rows then valid := false
  ) col_hints;
  !valid

(* Main entry point *)
let () =
  (* Define the hints for a 10x10 Nonogram *)
  let row_hints = [|
    [1];
    [3];
    [4];
    [3];
    [2]
  |] in
  let col_hints = [|
    [1];
    [1; 1];
    [3];
    [3];
    [4]
  |] in

  (* Grid size *)
  let rows = 5 in
  let cols = 5 in

  (* Validate hints *)
  if not (validate_hints row_hints col_hints rows cols) then (
    print_endline "Invalid hints for the given grid size."
  ) else (
    (* Initialize the grid *)
    let grid = initialize_grid rows cols in

    (* Solve the Nonogram *)
    let success, solution = solve_nonogram grid row_hints col_hints in

    if success then (
      print_endline "Solved Nonogram!";

      (* Finalize the grid by blocking remaining Empty cells *)
      let finalized_solution = finalize_grid solution in

      (* Print the finalized grid *)
      print_grid finalized_solution
    ) else (
      print_endline "No solution exists."
    )
  )
