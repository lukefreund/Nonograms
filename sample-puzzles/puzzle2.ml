let sample_puzzle = {
  row_constraints = [
    [5; 1; 1];
    [2; 5];
    [3; 1];
    [3; 4];
    [4; 2];
    [4; 2];
    [1; 1; 1; 3];
    [4];
    [2];
    [1];
  ];
  col_constraints = [
    [3; 2];
    [6];
    [1; 5];
    [1; 2];
    [1; 1; 2];
    [2; 1];
    [1; 1; 4];
    [2; 1; 4];
    [1; 4];
    [5];
  ];
}

(* Sample grid matching the constraints *)
let sample_grid = [
  [Empty; Filled; Filled; Filled; Filled; Filled; Empty; Filled; Empty; Filled];
  [Filled; Filled; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled];
  [Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty; Empty; Filled];
  [Filled; Filled; Filled; Empty; Empty; Empty; Filled; Filled; Filled; Filled];
  [Empty; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Filled; Filled];
  [Filled; Filled; Filled; Filled; Empty; Empty; Empty; Filled; Filled; Empty];
  [Filled; Empty; Filled; Empty; Filled; Empty; Filled; Filled; Filled; Empty];
  [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Empty];
  [Empty; Empty; Empty; Empty; Empty; Empty; Filled; Filled; Empty; Empty];
  [Empty; Empty; Empty; Empty; Empty; Empty; Filled; Empty; Empty; Empty];
]

(* Combine the puzzle and grid into a nonogram *)
let sample_nonogram = {
  puzzle = sample_puzzle;
  grid = sample_grid;
}