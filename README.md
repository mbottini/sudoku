# Sudoku Solver

## Mike Bottini

This is a Sudoku solver that implements Donald Knuth's Algorithm X to solve
Sudoku problems as an exact cover problem.

# Installing Haskell

Use [ghcup](https://www.haskell.org/ghcup/) to install Haskell. If on Linux,
you can also use the provided distro installer. You will also need `cabal`,
which is provided by default with the `ghcup` install but has to be installed
separately when managed by your package manager.

# Building the Project

    cabal build

# Installing (Optional)

    cabal install

Make sure that the install location is on your `PATH`. On Linux, by default
this will end up in `/home/$USER/.local/bin`.

# Running

    sudoku puzzle.txt       # opens puzzle.txt
    sudoku < puzzle.txt     # no filepath will read from stdin
    sudoku - < puzzle.txt   # a single dash will also read from stdin

Or, if you didn't install, run with Cabal with

    cabal run sudoku -- puzzle.txt

# Puzzle Format

A 9x9 grid of spaces and numbers. If no more numbers remain on a line, you can
end it early with a newline.

# Example

    $ cat puzzle.txt
    53  7
    6  195
     98    6 
    8   6   3
    4  8 3  1
    7   2   6
     6    28 
       419  5
        8  79
    $ time sudoku puzzle.txt 
    There is 1 solution
    534678912
    672195348
    198342567
    859761423
    426853791
    713924856
    961537284
    287419635
    345286179


    real	0m0.175s
    user	0m0.151s
    sys	0m0.016s

