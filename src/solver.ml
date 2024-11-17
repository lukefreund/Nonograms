(* solver.ml *)

open Nonogram

exception Contradiction

type cell_state =
  | Known of cell
  | Unknown

type line_possibilities = cell list list

type solving_state = {
  grid : cell_state array array;
  row_poss : line_possibilities array;
  col_poss : line_possibilities array;
}

(* Custom function to replace List.init *)
let rec list_init n f =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (f i :: acc)
  in
  aux (n - 1) []


let generate_line_possibilities constraints line_length =
  let rec aux constraints remaining_length =
    match constraints with
    | [] ->
      if remaining_length >= 0 then [ list_init remaining_length (fun _ -> Empty) ]
      else []
    | n :: rest ->
      let min_spaces = if rest = [] then 0 else 1 in
      let sum_rest_constraints = match rest with
        | [] -> 0
        | _ -> List.fold_left ( + ) 0 rest + List.length rest - 1
      in
      let max_prefix_spaces = remaining_length - (n + min_spaces + sum_rest_constraints) in
      if max_prefix_spaces < 0 then []
      else
        let possible_prefix_spaces = list_init (max_prefix_spaces + 1) (fun i -> i) in
        List.concat (List.map (fun spaces ->
          let prefix = list_init spaces (fun _ -> Empty) in
          let block = list_init n (fun _ -> Filled) in
          let suffix_length = remaining_length - spaces - n - min_spaces + if rest = [] then 1 else 0 in
          let suffix_possibilities = aux rest suffix_length in
          List.map (fun suffix ->
            prefix @ block @ (if rest = [] then [] else [Empty]) @ suffix
          ) suffix_possibilities
        ) possible_prefix_spaces)
  in
  List.filter (fun line -> List.length line = line_length) (aux constraints line_length)

let update_line_possibilities line_possibilities grid_line =
  List.filter (fun possibility ->
    List.for_all2 (fun cell_state cell ->
      match cell_state with
      | Known c -> c = cell
      | Unknown -> true
    ) grid_line possibility
  ) line_possibilities

let intersect_line_possibilities line_possibilities =
  match line_possibilities with
  | [] -> [||]  (* Return an empty array *)
  | _ ->
    let length = List.length (List.hd line_possibilities) in
    Array.init length (fun idx ->
      let cells_at_idx = List.map (fun poss -> List.nth poss idx) line_possibilities in
      if List.for_all (fun c -> c = List.hd cells_at_idx) cells_at_idx then
        Known (List.hd cells_at_idx)
      else
        Unknown
    )

let rec propagate_constraints state =
  let changed = ref false in
  let contradiction = ref false in

  (* Update rows *)
  for row = 0 to Array.length state.grid - 1 do
    let line_poss = update_line_possibilities state.row_poss.(row) (Array.to_list state.grid.(row)) in
    if line_poss = [] then contradiction := true;
    if List.length line_poss < List.length state.row_poss.(row) then (
      changed := true;
      state.row_poss.(row) <- line_poss;
    );
    if not !contradiction then (
      let new_line = intersect_line_possibilities line_poss in
      for col = 0 to Array.length state.grid.(0) - 1 do
        match new_line.(col), state.grid.(row).(col) with
        | Known c1, Unknown ->
          changed := true;
          state.grid.(row).(col) <- Known c1
        | _ -> ()
      done
    )
  done;

  (* Update columns *)
  for col = 0 to Array.length state.grid.(0) - 1 do
    let grid_column = Array.init (Array.length state.grid) (fun row -> state.grid.(row).(col)) in
    let line_poss = update_line_possibilities state.col_poss.(col) (Array.to_list grid_column) in
    if line_poss = [] then contradiction := true;
    if List.length line_poss < List.length state.col_poss.(col) then (
      changed := true;
      state.col_poss.(col) <- line_poss;
    );
    if not !contradiction then (
      let new_line = intersect_line_possibilities line_poss in
      for row = 0 to Array.length state.grid - 1 do
        match new_line.(row), state.grid.(row).(col) with
        | Known c1, Unknown ->
          changed := true;
          state.grid.(row).(col) <- Known c1
        | _ -> ()
      done
    )
  done;

  if !contradiction then
    raise Contradiction
  else if !changed then
    propagate_constraints state

let rec solve state =
  try
    propagate_constraints state;

    if Array.for_all (fun row ->
      Array.for_all (fun cell_state -> cell_state <> Unknown) row
    ) state.grid then
      Some (Array.map (Array.map (function Known c -> c | Unknown -> Empty)) state.grid)
    else
      let min_row, min_row_poss = ref (-1), ref max_int in
      for i = 0 to Array.length state.row_poss - 1 do
        let len = List.length state.row_poss.(i) in
        if len > 1 && len < !min_row_poss then (
          min_row := i;
          min_row_poss := len
        )
      done;
      let min_col, min_col_poss = ref (-1), ref max_int in
      for i = 0 to Array.length state.col_poss - 1 do
        let len = List.length state.col_poss.(i) in
        if len > 1 && len < !min_col_poss then (
          min_col := i;
          min_col_poss := len
        )
      done;

      if !min_row_poss <= !min_col_poss then
        branch_on_line state `Row !min_row
      else
        branch_on_line state `Col !min_col
  with Contradiction ->
    None

and branch_on_line state line_type index =
  let possibilities =
    match line_type with
    | `Row -> state.row_poss.(index)
    | `Col -> state.col_poss.(index)
  in
  let rec try_possibilities poss_list =
    match poss_list with
    | [] -> None
    | poss :: rest ->
      let new_state = {
        grid = Array.map Array.copy state.grid;
        row_poss = Array.copy state.row_poss;
        col_poss = Array.copy state.col_poss;
      } in
      (try
        (match line_type with
        | `Row ->
          new_state.grid.(index) <- Array.mapi (fun i cell_state ->
            match cell_state with
            | Known c ->
              if c = List.nth poss i then Known c
              else raise Contradiction
            | Unknown -> Known (List.nth poss i)
          ) new_state.grid.(index);
          new_state.row_poss.(index) <- [poss]
        | `Col ->
          for i = 0 to Array.length state.grid - 1 do
            new_state.grid.(i).(index) <- (match new_state.grid.(i).(index) with
              | Known c ->
                if c = List.nth poss i then Known c
                else raise Contradiction
              | Unknown -> Known (List.nth poss i)
            )
          done;
          new_state.col_poss.(index) <- [poss]
        );
        match solve new_state with
        | Some solution -> Some solution
        | None -> try_possibilities rest
      with Contradiction ->
        try_possibilities rest)
  in
  try_possibilities possibilities

let solve_nonogram_eff (puzzle : puzzle) : grid =
  let height = List.length puzzle.row_constraints in
  let width = List.length puzzle.col_constraints in
  let initial_state = {
    grid = Array.make_matrix height width Unknown;
    row_poss = Array.init height (fun i ->
      generate_line_possibilities (List.nth puzzle.row_constraints i) width
    );
    col_poss = Array.init width (fun i ->
      generate_line_possibilities (List.nth puzzle.col_constraints i) height
    );
  } in
  match solve initial_state with
  | Some solution -> Array.to_list (Array.map Array.to_list solution)
  | None -> failwith "No solution exists for the given puzzle."
