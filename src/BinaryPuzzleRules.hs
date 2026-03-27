-- These are the various rules we can apply to get closer to solving a BinaryPuzzle
module BinaryPuzzleRules (applyBasicRules) where 

import Data.List (isInfixOf, transpose)
import Data.String.Utils
import BinaryPuzzleGrids

-- Given a grid, apply rules to it recursively until we cannot
-- apply any futher rules
applyBasicRules :: Grid -> Grid
applyBasicRules g 
    | thisRun == g = g -- can't make further progress
    | otherwise    = applyBasicRules thisRun
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
apLeftIdRules n g
    | n > (length g `div` 2) = g
    | otherwise              = apLeftIdRules (n-1) (identicalLeftRule n g)

-- If the remaining n spaces are all blank and equal to the difference
-- between the counts of 1s and 0s then fill the blanks with the digit of
-- which there are fewest. Or if not just pass the unchanged grid back.
identicalLeftRule :: Int -> Grid -> Grid
identicalLeftRule n =
    map repl
  where
    repl r
      |  bLen r == n && abs (zeroLen r - oneLen r) == n
        = if zeroLen r > oneLen r
          then replaceIfSafe "_" "1" r
          else replaceIfSafe "_" "0" r
      | otherwise
        = r

-- Wrapper round Data.String.Utils.replace that prevents adding triples in the
-- identical left rules
replaceIfSafe :: String -> String -> Row -> Row
replaceIfSafe p s r
    | (s++"__") `isInfixOf` r || ("__" ++ s) `isInfixOf` r -- lets not introduce a triple!
                = r
    | otherwise = replace p s r

-- Just a convenience for replacing a row in a grid
replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex n x xs
    | n < 0 || n > length xs - 1 = error "Index out of bounds"
    | otherwise                  = take n xs ++ x : drop (n+1) xs

-- If there are 2 spaces left in a row then try filling with 1,0
-- or 0,1 and see if we produce a safeGrid, If we do then we can
-- pass the modified  row back, if not, the original.
twoDifferentLeftRule :: Grid -> Grid
twoDifferentLeftRule g =
    map (twoDifferentLeftRule' g) $ zip [0..] g

-- We try 1,0 and 0,1 when there are two spaces left and we know that they
-- are both different.
-- If they are both valid then return the original row
-- if one is and one isn't then return the valid one
twoDifferentLeftRule' :: Grid -> (Int,Row) -> Row
twoDifferentLeftRule' g (i,r)
    | length rowParts /=3 = r
    | otherwise           =
        case ( safe (newGrid '0' '1'), safe (newGrid '1' '0') ) of
        (True, False) -> newRow '0' '1'
        (False, True) -> newRow '1' '0'
        (_,_)         -> r -- error "We don't have a better solution"
  where
    rowParts    = split "_" r
    newGrid n m = replaceIndex i (newRow n m) g
    newRow n m  = headS rowParts ++ (n : rowParts !! 1) ++ m : (rowParts !! 2)
