{-# LANGUAGE OverloadedStrings #-}
-- This code is disgusting. But I don't really know what I am doing with tag soup.
module BinaryPuzzleGetFromWeb (getFromWeb) where

import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Network.HTTP.Simple
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as L8

import BinaryPuzzleGrids (Grid, printGrid)

-- Get a puzzle from the binary puzzles site
getFromWeb :: String -> IO Grid
getFromWeb url = do
    response <- httpLBS (parseRequest_ url)
    let body  = getResponseBody response
        tags  = parseTags (L8.unpack body)
        cells = extractCells tags
        size  = inferSize (length cells)
        grid  = chunk size cells
    return grid

-- Write the puzzle from the binary puzzles site
getFromWebAndMakeFile :: String -> String -> IO ()
getFromWebAndMakeFile puzzleUrl outFile = do
  grid <- getFromWeb puzzleUrl
  writeFile outFile (unlines grid)
  putStrLn $ "Puzzle saved to: " ++ outFile

-- Just my test
exampleGetFromWebAndMakeFile =
  getFromWebAndMakeFile "https://www.binarypuzzle.com/puzzles.php?size=14&level=3&nr=8" "test/webpuz.txt"

extractCells :: [Tag String] -> [Char]
extractCells tags =
    [ extractValue block
    | block <- partitions isPuzzleDiv tags
    ]

isPuzzleDiv :: Tag String -> Bool
isPuzzleDiv (TagOpen "div" attrs) =
    case lookup "id" attrs of
      Just cid -> "cel_" `isInfixOf` cid
      _        -> False
isPuzzleDiv _ = False

extractValue :: [Tag String] -> Char
extractValue ts =
    case dropWhile (~/= TagOpen ("p"::String) []) ts of
        (TagOpen "p" _ : TagText txt : _) ->
            case trim txt of
                "0" -> '0'
                "1" -> '1'
                _   -> '_'
        _ -> '_'

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` (" \n\t\r"::String))

inferSize :: Int -> Int
inferSize total =
    floor (sqrt (fromIntegral total :: Double))

chunk :: Int -> [a] -> [[a]]
chunk n xs
    | null xs   = []
    | otherwise = take n xs : chunk n (drop n xs)
