module BinaryPuzzle (Grid, solve, printGrid, solvedGrid) where

import Control.Monad (replicateM)
import Data.List (isInfixOf,transpose,group,sort)
import Data.Ord (comparing)
import Data.String.Utils

type Row    = String
type Grid   = [Row]
type Matrix = [Grid]

unsolvableGrid :: Grid
unsolvableGrid =
    [ "_______0"
    , "10010110"
    , "_0___1_0"
    , "__1_____"
    , "00_1__1_"
    , "____1___"
    , "11___0_1"
    , "_1_____1"
    ]

main :: IO()
main = printGrid $ solve g14'

-- max problem space to look at with dumb algorithim
maxPSDumb = 1000

-- Calculate the problem space of a matrix - how many rows you would need to try
-- if you just took the cartesian product, we use this to see if we can get away
-- with just filtering the cartesian product for valid solutions
problemSpace :: Matrix -> Int
problemSpace = product . map length


-- head that returns empty for an empty list
headS [] = []
headS xs = head xs

-- First we apply deterministic rules so that we can reduce the
-- problem space as much as possible. Then we generate a "matrix" of
-- possible rows for the remaining problem space.
-- If there aren't that many then we can solve quickly by just
-- searching through the cartesian product of the matrix for valid 
-- solutions
-- Otherwise, we call solveSlow
solve :: Grid -> Grid
solve g =
    if probSpace < maxPSDumb
    then headS . filter solvedGrid $ cp' pgs
    else solveSlow (betterGrid g pgs)
  where
    pgs       = possibles g
    probSpace = problemSpace pgs

-- This way we try to be a bit smarter about trimming the solution space
-- as we go. This can solve more complex puzzles, but is slower on small
-- puzzles.
solveSlow :: Grid -> Grid
solveSlow g =
    concat . headS $ trim g

-- Cartesian product of a matrix - used in our slow algorithm to construct all
-- possible grids from a matrix
cp' :: Matrix -> [Grid]
cp' [] = [[]]
cp' (ms:mss) =
    [y:ys | ys <- yss, y <- ms]
  where
    yss = cp' mss

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
    betterChoices = if allSingle ps
                    then [ps]              -- we have a valid solution!
                    else filter (safe . betterGrid g)
                         $ expandMatrix ps -- we need to trim the remaining matrices

-- Are all the rows in a matrix single, indicating a possible solution
allSingle :: Matrix -> Bool
allSingle = all ((==1) . length)

-- Given a matrix, return a list of matrices where each of the variants of the smallest
-- (non-single) row of the original matrix is installed in one of the new matrices
expandMatrix :: Matrix -> [Matrix]
expandMatrix rows =
    if null $ counts rows
    then [rows]
    else [ row1 ++ ([c] : row2) | c <- cs]
  where
    (row1, cs : row2) = break smallest rows
    smallest cs'      = length cs' == n
    n                 = minimum (counts rows)
    counts            = filter (/= 1) . map length

-- Can we build a better grid with any of the rows from a matrix?
-- ie. does the matrix have any grids with a row of length 1 that is not in
-- our grid
-- If so return the better grid, otherwise return the original
betterGrid :: Grid -> Matrix -> Grid
betterGrid [] _ = []
betterGrid _ [] = []
betterGrid (g:gs) (m:ms) =
    if length m == 1
    then head m : betterGrid gs ms
    else g : betterGrid gs ms

-- Given a grid, returns a Matrix, which is a list of possible rows for
-- each row of a matrix
possibles :: Grid -> Matrix
possibles g =
      if g == g2
      then if all (not . null) rs then rs else []
      else possibles g2
    where
      g0 = applyDeterministicRules g
      ns = nLengthRows (length g0)
      cs = candidates (cols g0) ns
      g1 = cols $ betterGrid (cols g0) cs
      rs = uniqueOnSingles $ candidates g0 ns
      g2 = betterGrid g1 rs

-- Get all possible valid rows for a grid of length n
nLengthRows :: Int -> Grid
nLengthRows n =
    if even n
    then filter validRow $ replicateM n "10"
    else error "Only even sized grids make sense"

-- Given a grid to test and a list of rows, for each row
-- in the test row, pull out all the rows that could match
-- that row from the list of rows
candidates :: Grid -> Grid -> [Grid]
candidates g g' =
    map (\r -> filter (rowMatches r) g') g

-- Given 2 rows, for each cell of the first check that is empty
-- OR that if it is a 1 or 0 it is equal to the same cell in the
-- other row
rowMatches :: Row -> Row -> Bool
rowMatches [] []         = True
rowMatches (r:rs) (x:xs) = (r=='_' || r==x) && rowMatches rs xs
rowMatches _ _           = error "Row of different length encountered"

-- If a solved row exists -- that is a row with a single candidate,
-- it should be excluded from the possible solutions
uniqueOnSingles :: Matrix -> Matrix
uniqueOnSingles ms =
    map (\m -> if length m > 1
               then filter (`notElem` us) m
               else m) ms
  where
    us = concat $ filter ((==1) . length) ms

-- Given a grid, return the columns of the grid
cols :: Grid -> Grid
cols = transpose

-- Given a grid, apply rules to it recursively until we cannot
-- apply any futher rules
applyDeterministicRules :: Grid -> Grid
applyDeterministicRules g =
    if thisRun == g
    then g
    else applyDeterministicRules thisRun
  where
    thisRun   = transpose . apCols $ apRows g
    apCols rs = apRows (cols rs)
    apRows rs = sandwichRule
              . twoDifferentLeftRule
              . doubleDigitRule
              $ apLeftIdRules (length rs `div` 2) rs

-- If I see 1_1 I can replace it with 101
sandwichRule :: Grid -> Grid
sandwichRule =
    map (replace "0_0" "010" . replace "1_1" "101")

-- If I see _11 I can replace it with 011 and similarly for 11_ -> 110
doubleDigitRule :: Grid -> Grid
doubleDigitRule =
    map ( replace "00_" "001"
        . replace "11_" "110"
        . replace "_00" "100"
        . replace "_11" "011"
        )

-- These are rules of the form:
-- If there are two spaces left and I have a difference of two between
-- 0s and 1s I can infer that all the remaining spaces should be filled
-- with the digit of which there are fewer
apLeftIdRules :: Int -> Grid -> Grid
apLeftIdRules 0 g = g
apLeftIdRules n g =
    if n > (length g `div` 2)
    then g
    else apLeftIdRules (n-1) (identicalLeftRule n g)

identicalLeftRule :: Int -> Grid -> Grid
identicalLeftRule n =
    map repl
  where
    repl r =
      if bLen r == n && abs (zeroLen r - oneLen r) == n
      then if zeroLen r > oneLen r
           then replaceIfSafe "_" "1" r
           else replaceIfSafe "_" "0" r
      else r

-- Wrapper round Data.String.Utils.replace that prevents adding triples in the
-- identical left rules
replaceIfSafe :: String -> String -> Row -> Row
replaceIfSafe p s r =
    if   (s++"__") `isInfixOf` r
      || ("__" ++ s) `isInfixOf` r -- lets not introduce a triple!
    then r
    else replace p s r

twoDifferentLeftRule :: Grid -> Grid
twoDifferentLeftRule g =
    map (twoDifferentLeftRule' g) $ zip [0..] g

-- We try 1,0 and 0,1 when there are two spaces left and we know that they
-- are both different.
-- If they are both valid then return the original row
-- if one is and one isn't then return the valid one
twoDifferentLeftRule' :: Grid -> (Int,Row) -> Row
twoDifferentLeftRule' g (i,r) =
    if length rowParts /=3
    then r
    else case ( safe (newGrid '0' '1')
              , safe (newGrid '1' '0') ) of
        (True, False) -> newRow '0' '1'
        (False, True) -> newRow '1' '0'
        (_,_)         -> r -- error "We don't have a better solution"
  where
    rowParts = split "_" r
    newGrid n m = replaceIndex i (newRow n m) g
    newRow n m = head rowParts ++ (n : rowParts !! 1) ++ m : (rowParts !! 2)

-- Just a convenience for replacing a row in a grid
replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex n x xs =
    if n < 0 || n > length xs - 1
    then error "Index out of bounds"
    else take n xs ++ x : drop (n+1) xs


zeroLen, oneLen, nbLen, bLen :: Row -> Int
zeroLen   = length . zeros
oneLen    = length . ones
nbLen     = length . nonBlanks
bLen      = length . blanks

zeros, ones, nonBlanks, blanks :: Row -> Row
zeros     = filter (=='0') . nonBlanks
ones      = filter (=='1') . nonBlanks
nonBlanks = filter (/='_')
blanks    = filter (=='_')

-- Grid is not null
-- has all columns and rows unique
-- has all columns and rows valid (i.e no triples 0s, 1s same)
solvedGrid :: Grid -> Bool
solvedGrid g =
    not (null g) && uniqGrid g && validRows g && validRows (cols g)

-- A uniqGrid has all columns and all rows unique
uniqGrid :: Grid -> Bool
uniqGrid g = uniqRows g && uniqRows (cols g)

uniqRows :: Grid -> Bool
uniqRows g = length g == length (group $ sort g)

validRows :: Grid -> Bool
validRows = all validRow

validRow :: Row -> Bool
validRow g =
    (oneLen g == zeroLen g) && noTriples g

noTriples :: Row -> Bool
noTriples g = not ("111" `isInfixOf` g || "000" `isInfixOf` g)

printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn

-- a safe grid is one that doesn't contain triples, 
-- and in which the counts of ones and zeros are the same for non-blank rows
-- and for which no row is empty
safe :: Grid -> Bool
safe g =  all noTriples (cols g)
       && all noTriples g
       && all nonempty g

oneZeroSameIfNotBlank r = nbLen r == 0 || oneLen r == zeroLen r

nonempty :: Row -> Bool
nonempty [] = False
nonempty _ = True

-- binarypuzzle.com 14^2 v.hard board 1
g14'  = [ "____10_______1"
        , "1____0___1_1_1"
        , "__1___1_______"
        , "0____11____0_1"
        , "0_0_____1____1"
        , "_0______0_____"
        , "__1______1__1_"
        , "__1_0__1__0___"
        , "_____1_1____1_"
        , "0_10_1____0___"
        , "__1_______00__"
        , "___0__________"
        , "_1_______0____"
        , "__0___0____0__"
        ]
