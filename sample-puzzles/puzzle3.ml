let sample_puzzle = {
  row_constraints = [
    [2];
    [2];
    [3];
    [3];
    [3];
  ];
  col_constraints = [
    [1; 1];
    [2; 2];
    [4];
    [2];
    [1];
  ];
}

(* Sample grid matching the constraints *)
let sample_grid = [
  [Filled; Filled; Empty; Empty; Empty];
  [Empty; Filled; Filled; Empty; Empty];
  [Empty; Empty; Filled; Filled; Filled];
  [Empty; Filled; Filled; Filled; Empty];
  [Filled; Filled; Filled; Empty; Empty];
]

(* Combine the puzzle and grid into a nonogram *)
let sample_nonogram = {
  puzzle = sample_puzzle;
  grid = sample_grid;
}