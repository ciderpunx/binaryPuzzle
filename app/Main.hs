module Main where

import System.Environment (getArgs)
import Data.List (isPrefixOf)
import BinaryPuzzle

main :: IO ()
main = do
  args <- getArgs
  case args of
      [filename] -> do
           grid <- getGrid filename
           printGrid $ solve grid
      _ -> putStrLn "Usage: binaryPuzzle <filename>"

getGrid :: String -> IO Grid
getGrid filename
  | "https://www.binarypuzzle.com/puzzles.php" `isPrefixOf` filename
    = getFromWeb filename
  | otherwise
    = readGrid filename


example :: IO ()
example = printGrid $ solve g

-- From a dutch puzzle book 0.5 seconds in ghci
g :: Grid
g =   [ "_1_00_________"
      , "____10__11__01"
      , "_1_________1__"
      , "0__1___1______"
      , "_______1______"
      , "___1_1_______0"
      , "__0__1____0_1_"
      , "1_0____1___11_"
      , "_1__0__1_1____"
      , "________0_1_1_"
      , "_0_____1___11_"
      , "___0_1______0_"
      , "__0_1_0_11_0_0"
      , "0_____________"
      ]
