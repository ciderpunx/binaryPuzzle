-- Deals with opening files and trying to pull a binary puzzle Grid out of them
module BinaryPuzzleFileReader (readGrid) where

import System.IO.Error (catchIOError)
import BinaryPuzzleGrids (Grid)

-- Give me a filepath and I'll give you Either a Grid or an IO Error
readGrid :: FilePath -> IO Grid
readGrid filePath = do
    content <- readFile filePath `catchIOError` handleIOError
    case feasibleGrid (lines content) of
        Left err   -> ioError $ userError err
        Right grid -> return grid
  where
    handleIOError e = ioError $ userError $ "File error: " ++ show e ++ "\n\n"

-- The grid ought to be made of only 1,0, or _ and should have an equal number of equal length lines
-- Otherwise it isn't feasible that it is a valid grid.
feasibleGrid :: [String] -> Either String Grid
feasibleGrid [] = Left "File is empty"
feasibleGrid lines@(firstLine:_)
  | any (/= lineLength) lineLengths = Left "Lines have inconsistent lengths"
  | any (not . even) lineLengths    = Left "Lines must be even length"
  | numLines /= lineLength          = Left $ "Grid must be equal. Got: " ++ show numLines
                                      ++ " lines of length " ++ show lineLength
  | any (not . all validChar) lines = Left "Invalid character encountered. I can only deal with 1,0 and _"
  | otherwise                       = Right lines
    where
      lineLengths = map length lines
      numLines    = length lines
      lineLength  = length firstLine
      validChar c = c == '1' || c == '0' || c == '_'
