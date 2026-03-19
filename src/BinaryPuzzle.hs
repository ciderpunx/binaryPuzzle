module BinaryPuzzle (Grid, solve, printGrid, readGrid, getFromWeb, solvedGrid) where

import BinaryPuzzleSolveGrids
import BinaryPuzzleFileReader
import BinaryPuzzleGetFromWeb

solve :: Grid -> Grid
solve = solveGrid
