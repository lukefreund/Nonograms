open Nonogram

(* Function to print the nonogram *)
let print_nonogram nonogram =
  let { puzzle = { row_constraints; col_constraints }; grid } = nonogram in

  (* Determine the maximum number of constraints in columns and rows *)
  let max_row_constraints = List.fold_left (fun acc rc -> max acc (List.length rc)) 0 row_constraints in
  let max_col_constraints = List.fold_left (fun acc cc -> max acc (List.length cc)) 0 col_constraints in

  (* Prepare the column constraints for printing *)
  let col_constraints_transposed =
    let padded_col_constraints = List.map (fun cc ->
      let padding = max_col_constraints - List.length cc in
      List.init padding (fun _ -> "") @ List.map string_of_int cc
    ) col_constraints in
    (* Transpose the column constraints *)
    let rec transpose lists =
      if List.exists (fun l -> l <> []) lists then
        let heads = List.map (function | [] -> "" | h :: _ -> h) lists in
        let tails = List.map (function | [] -> [] | _ :: t -> t) lists in
        heads :: transpose tails
      else
        []
    in
    transpose padded_col_constraints
  in

  (* Print the column constraints *)
  List.iter (fun row ->
    (* Add padding for row constraints *)
    let padding = String.make (max_row_constraints * 2) ' ' in
    print_string padding;
    (* Print the column constraints *)
    List.iter (fun c ->
      print_string (if c = "" then "  " else Printf.sprintf "%2s" c)
    ) row;
    print_newline ()
  ) col_constraints_transposed;

  (* Print the grid with row constraints *)
  List.iter2 (fun row_cells row_constraint ->
    (* Print row constraints *)
    let padded_row_constraint = 
      let padding = max_row_constraints - List.length row_constraint in
      List.init padding (fun _ -> "  ") @ List.map (fun n -> Printf.sprintf "%2d" n) row_constraint
    in
    List.iter print_string padded_row_constraint;
    (* Separator between constraints and grid *)
    print_string " |";
    (* Print the cells *)
    List.iter (fun cell ->
      let cell_str = match cell with
        | Filled -> "██"
        | Empty -> "  "
      in
      print_string cell_str
    ) row_cells;
    print_newline ()
  ) grid row_constraints

 