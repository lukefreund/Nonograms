# Nonogram Solver Project

## Team
Luke Freund, 261118003

Liam Di Chiro, 261118442

Selin Ida Omur, 261017800


## Overview

This project is an implementation of a Nonogram puzzle generator and solver in OCaml. Nonograms are logic puzzles where you fill in cells on a grid according to numerical clues provided for each row and column to reveal a hidden image.

## Features
Random Puzzle Generation: Create random Nonogram puzzles of any specified size.

Two Solving Algorithms: Compare different solving strategies and their efficiencies.

Performance Analysis: Measure execution time of solvers to analyze scalability.

User Interaction: Command-line interface for an interactive experience.

## Installation
### Prerequisites
OCaml (version 5.2.0)

Dune (version 3.16.1)

Clone the Repository

`git clone https://github.com/lukefreund/Nonograms.git`

`cd nonogram-solver`

## Building the Project

Use Dune to build the project:

`dune build`
If you encounter any issues related to the Unix module, ensure that your dune file includes (libraries unix) in the executable stanzas.

## Usage

Execute the main program to generate and solve a random Nonogram puzzle:

`dune exec ./src/main.exe`

## Try implementing the algorithms with the template code!
We have provided two files in the template_code folder, so that you can attempt to implement the Nonogram solving algorithms yourself. These come with function signatures and hints. You can compare your solution to our solution, which is in the src folder.

## Documentation used to help implement the code:

https://ocaml.org/manual/5.2/api/Array.html 

https://ocaml.org/docs/arrays#:~:text=In%20OCaml%2C%20arrays%20are%20collections,to%20elements%20at%20any%20position 

https://ocaml.org/docs/libraries-dune

https://ocaml.org/exercises?difficulty_level=advanced

