open Nonogram

(* Helper function to compute the maximum length of lists in a list of lists *)
let max_list_length lists =
  List.fold_left (fun acc lst -> max acc (List.length lst)) 0 lists

(* Pads a list with Nones on the left to reach the desired length *)
let pad_left length lst =
  let padding = length - List.length lst in
  let rec make_padding n acc =
    if n <= 0 then acc else make_padding (n - 1) (None :: acc)
  in
  make_padding padding [] @ (List.map (fun x -> Some x) lst)

(* Corrected transpose function *)
let transpose lists =
  let rec transpose_aux lists =
    if List.for_all (fun lst -> lst = []) lists then []
    else
      let heads = List.map (function [] -> None | h :: _ -> h) lists in
      let tails = List.map (function [] -> [] | _ :: t -> t) lists in
      heads :: transpose_aux tails
  in
  transpose_aux lists

(* The main function to print the nonogram *)
let print_nonogram (nonogram : nonogram) : unit =
  let grid = nonogram.grid in
  let row_constraints = nonogram.puzzle.row_constraints in
  let col_constraints = nonogram.puzzle.col_constraints in

  (* Compute max lengths *)
  let max_row_constraints_length = max_list_length row_constraints in
  let max_col_constraints_length = max_list_length col_constraints in

  (* Pad the constraints *)
  let padded_col_constraints =
    List.map (pad_left max_col_constraints_length) col_constraints
  in
  let padded_row_constraints =
    List.map (pad_left max_row_constraints_length) row_constraints
  in

  (* Transpose the column constraints *)
  let transposed_col_constraints = transpose padded_col_constraints in

  (* Print the column constraints *)
  List.iter
    (fun line ->
      (* Print spaces for row constraints *)
      for _ = 1 to max_row_constraints_length do
        print_string "  "
      done;
      (* Print the column constraints *)
      List.iter
        (function
        | None -> print_string "  "
        | Some n -> Printf.printf "%2d" n)
        line;
      print_newline ())
    transposed_col_constraints;

  (* Now, print each row *)
  List.iter2
    (fun row_constraint row ->
      (* Print the row constraints *)
      List.iter
        (function
        | None -> print_string "  "
        | Some n -> Printf.printf "%2d" n)
        row_constraint;
      (* Print the cells *)
      List.iter
        (fun cell ->
          match cell with
          | Filled -> print_string "██"
          | Empty -> print_string "  ")
        row;
      print_newline ())
    padded_row_constraints grid
