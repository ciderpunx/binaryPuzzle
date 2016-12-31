module Grids0 where
import BinaryPuzzle
-- Some test grids from various places

testGrid :: Grid
testGrid =
    [ "101010"
    , "010011"
    , "100101"
    , "011010"
    , "001101"
    , "110100"
    ]

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

g6 :: Grid
g6 =
    [ "1__0__"
    , "__00_1"
    , "_0___1"
    , "______"
    , "00_1__"
    , "_1__00"
    ]

g8 :: Grid
g8 =
    [ "_______0"
    , "_00__1__"
    , "_0___1_0"
    , "__1_____"
    , "00_1__1_"
    , "____1___"
    , "11___0_1"
    , "_1_____1"
    ]

g8' :: Grid
g8' =
    [ "_______0"
    , "10010101"
    , "_0___1_0"
    , "__1_____"
    , "00_1__1_"
    , "____1___"
    , "11___0_1"
    , "_1_____1"
    ]

g10 :: Grid
g10 =
    [ "_______1__"
    , "_00__0__1_"
    , "_0__1__0_0"
    , "__1___1___"
    , "1_1______1"
    , "_______1__"
    , "_0__1___0_"
    , "____11___0"
    , "_0_0__1__0"
    , "0___0___1_"
    ]

g12 :: Grid
g12 =
    [ "__1____0__0_"
    , "___1_0__1__1"
    , "__0_0____0_1"
    , "1_0___00____"
    , "_1_________1"
    , "____1____0__"
    , "______1_00__"
    , "__0____1__1_"
    , "1_00_____0__"
    , "_____1___0_0"
    , "_0____0___1_"
    , "00_1_1_1_1__" 
    ]

-- binarypuzzle.com 12^2 V.hard board 1 solves in under a second on ghci
g12' :: Grid
g12' =
    [ "_______00_1_"
    , "__1_______1_"
    , "___0_0__0__0"
    , "_____0______"
    , "_11_1__1_0__"
    , "_11_0_00____"
    , "__________1_"
    , "1__00___0___"
    , "____________"
    , "__1_____1___"
    , "___0__0__0_0"
    , "__10___0____"
    ]

-- From a dutch puzzle book 0.5 seconds in ghci
g14 :: Grid
g14 =   [ "_1_00_________"
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

-- binarypuzzle.com 14^2 v.hard board 1
-- REST:
-- https://query.yahooapis.com/v1/public/yql?q=select%20content%20from%20html%20where%20url%3D'http%3A%2F%2Fwww.binarypuzzle.com%2Fpuzzles.php%3Fsize%3D14%26level%3D4%26nr%3D1'%20and%20xpath%3D'%2F%2Fdiv%5B%40class%3D%22puzzlecel%22%5D%2Fp'&format=json&diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=
-- Solvable in 21 seconds in ghci or 4ish if you compile
g14' :: Grid
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
