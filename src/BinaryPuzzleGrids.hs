-- Rows, Grids or Matrixes and the operations you may do on them
module BinaryPuzzleGrids
  (Grid
  , Row
  , Matrix
  , allSingle
  , bLen
  , cols
  , cp'
  , headS
  , oneLen
  , printGrid
  , safe
  , uniqGrid
  , validRow
  , validRows
  , zeroLen
  ) where

import Data.List (transpose, isInfixOf, nub)
import Data.Maybe (listToMaybe)

type Row    = String
type Grid   = [Row]
type Matrix = [Grid]

-- Head but returns empty list if given an empty list
-- Not strictly a Row operation but used a lot dealing with Rows.
headS = maybe [] id . listToMaybe

-- Cartesian product of a matrix - used in our slow algorithm to construct all
-- possible grids from a matrix
cp' :: Matrix -> [Grid]
cp' = foldr (\xs acc -> [x:ys | x <- xs, ys <- acc]) [[]]

-- Given a grid, return the columns of the grid
cols :: Grid -> Grid
cols = transpose

-- Print the content of a grid
printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn

-- A uniqGrid has all columns and all rows unique
uniqGrid :: Grid -> Bool
uniqGrid g = uniqRows g && uniqRows (cols g)

-- Are all Rows in a Grid unique?
uniqRows :: Grid -> Bool
uniqRows g = length g == length (nub g)

-- Are all Rows in a Grid "valid"
validRows :: Grid -> Bool
validRows = all validRow

-- Is a Row valid - number of 1s == number of 0s and there are no triples (111,000)
validRow :: Row -> Bool
validRow g = (oneLen g == zeroLen g) && noTriples g

-- Number of 0s,1s,_s resp.
zeroLen, oneLen, bLen :: Row -> Int
zeroLen   = length . zeros
oneLen    = length . ones
bLen      = length . blanks

-- List of 0s,1s,non-_s,_s, resp.
zeros, ones, nonBlanks, blanks :: Row -> Row
zeros     = filter (=='0') . nonBlanks
ones      = filter (=='1') . nonBlanks
nonBlanks = filter (/='_')
blanks    = filter (=='_')

-- A "safe" grid is one that doesn't contain triples,
-- and in which the counts of ones and zeros are the same for non-blank rows
-- and for which no row is empty
safe :: Grid -> Bool
safe g = all noTriples (cols g)
      && all noTriples g
      && all nonempty g

-- Given a Row return True if no triples in Row, False otherwise.
noTriples :: Row -> Bool
noTriples g = not ("111" `isInfixOf` g || "000" `isInfixOf` g)

-- There is at least a single 1 or 0 in the Row
nonempty :: Row -> Bool
nonempty = not . null

-- Are all the rows in a matrix single? Indicates a possible solution
allSingle :: Matrix -> Bool
allSingle = all ((==1) . length)
