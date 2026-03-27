-- Deal with solving or making progress towards solving Grids, Matrixes
module BinaryPuzzleSolveGrids (Grid, solveGrid, solvedGrid, printGrid) where

import Control.Monad (replicateM)
import BinaryPuzzleGrids
import BinaryPuzzleRules

-- A Grid is solved iff:
-- * Grid is not null
-- * has all columns and rows unique
-- * has all columns and rows valid (i.e no triples 0s, 1s same)
solvedGrid :: Grid -> Bool
solvedGrid g = not (null g)
            && uniqGrid g
            && validRows g
            && validRows (cols g)

-- Max problem space to look at with dumb algorithim
-- TODO: make into config param
maxPSDumb :: Int
maxPSDumb = 1000

-- Calculate the problem space of a matrix - how many rows you would need to try
-- if you just took the cartesian product, we use this to see if we can get away
-- with just filtering the cartesian product for valid solutions
problemSpace :: Matrix -> Int
problemSpace = product . map length

-- First we apply deterministic rules so that we can reduce the
-- problem space as much as possible. Then we generate a "matrix" of
-- possible rows for the remaining problem space.
-- If there aren't that many then we can solve quickly by just
-- searching through the cartesian product of the matrix for valid 
-- solutions
-- Otherwise, we call solveSlow
solveGrid :: Grid -> Grid
solveGrid g
    | probSpace < maxPSDumb = headS . filter solvedGrid $ cp' pgs
    | otherwise             = solveSlow (betterGrid g pgs)
  where
    pgs       = possibles g
    probSpace = problemSpace pgs

-- This way we try to be a bit smarter about trimming the solution space
-- as we go. This can solve more complex puzzles, but is slower on small
-- puzzles.
solveSlow :: Grid -> Grid
solveSlow = concat . headS . trim

-- The idea here is to reduce the number of possible grids we need to search
-- Given a grid, use expandMatrix to generate the possible solutions for that grid as a matrix
-- If they are all single then we have a possible solution, so return that
-- If not, then expand the search space by calling expandMatrix to create a list of matrices
--    then recursively trim them on better grids that we can generate from them
trim :: Grid -> [Matrix]
trim [] = []
trim g =
    case length betterChoices of
      0 -> []             -- no better matrix found, just return possibles for grid
      1 -> betterChoices  -- we found a single solution: the possibles of g, return that
      _ -> filter (\g -> not (null g) && solvedGrid (concat g))
           $ concatMap (trim . betterGrid g) betterChoices -- some additional valid matrices found, recurse
  where
    ps            = possibles g
    betterChoices
      | allSingle ps = [ps]              -- we have a valid solution!
      | otherwise    = filter (safe . betterGrid g)
                     $ expandMatrix ps   -- we need to trim the remaining matrices

-- Given a matrix, return a list of matrices where each of the variants of the smallest
-- (non-single) row of the original matrix is installed in one of the new matrices
expandMatrix :: Matrix -> [Matrix]
expandMatrix rows
    | null $ counts rows = [rows]
    | otherwise          = [ row1 ++ ([c] : row2) | c <- cs]
  where
    (row1, cs : row2) = break smallest rows
    smallest cs'      = length cs' == n
    n                 = minimum (counts rows)
    counts            = filter (/= 1) . map length

-- Can we build a better grid with any of the rows from a matrix?
-- ie. does the matrix have only a single grid in it
-- If so return the first row in the grid, otherwise return the original row
betterGrid :: Grid -> Matrix -> Grid
betterGrid =
    zipWith hwelce
  where
    hwelce g [x] = x
    hwelce g _   = g

-- Given a grid, returns a Matrix, which is a list of possible solutions for
-- each row of a matrix
possibles :: Grid -> Matrix
possibles g
      | g == g2   = if all (not . null) rs then rs else []
      | otherwise = possibles g2
    where
      g0 = applyBasicRules g
      ns = nLengthRows (length g0)
      cs = candidates (cols g0) ns
      g1 = cols $ betterGrid (cols g0) cs
      rs = uniqueOnSingles $ candidates g0 ns
      g2 = betterGrid g1 rs

-- Get all possible valid rows for a grid of length n
nLengthRows :: Int -> Grid
nLengthRows n
  | even n    = filter validRow $ replicateM n "10"
  | otherwise = error "Only even sized grids make sense"

-- Given a grid to test and a list of rows, for each row
-- in the test grid, pull out all the rows that could match
-- it from the list of rows
candidates :: Grid -> Grid -> Matrix
candidates g g' =
    map (\r -> filter (rowMatches r) g') g

-- Given 2 rows, for each cell of the first check that is empty
-- OR that if it is a 1 or 0 it is equal to the same cell in the
-- other row
rowMatches :: Row -> Row -> Bool
rowMatches [] []         = True
rowMatches (r:rs) (x:xs) = (r=='_' || r==x) && rowMatches rs xs
rowMatches _ _           = error "Row of different length encountered"

-- Remove duplicate single solutions for rows from matrix
uniqueOnSingles :: Matrix -> Matrix
uniqueOnSingles ms =
    map (\m -> if length m > 1
               then filter (`notElem` us) m
               else m) ms
  where
    us = concat $ filter ((==1) . length) ms
