This is a Haskell program to solve binary puzzles, or binary sudoku like those you can find at [binarypuzzle.com](http://www.binarypuzzle.com/index.php).

## Run on Debian

You need stack installed

```
# apt install haskell-stack
```

Then git clone:

```
git clone https://github.com/ciderpunx/binaryPuzzle.git
```

Then run with stack. The first run will install many things.

```
$ stack run
Usage: binaryPuzzle <filename>
```

There is a test file in the test directory.

```
$ stack run test/testGridFile.txt 
01011010101001
10011001010101
10100110101010
01100110011001
01011001100101
10011011001010
01100100110110
00110011010101
11001101001010
00101100110101
10110010110010
11001001001101
01100110100110
10010101011010
```

The solved Binary Puzzle is from a Dutch puzzle book that I came across. You have to input puzzles manually in app/Main (which is quite annoying) I will maybe make a nice way to input puzzles one day.

The [BinaryPuzzle.com](https://www.binarypuzzle.com/) site has a bunch of puzzles, updated daily. You can provide a URL from that site (limited to the "/puzzles.php" ones for now) to solve it. This is a bit beta and untested!

```
stack run "https://www.binarypuzzle.com/puzzles.php?size=14&level=3&nr=8"
11010011001100
00110110011001
00101101100110
11001001010011
10010110101001
01101001100110
01101001010101
10010110011010
01100110101010
10101001010101
11010010101100
00110100110011
01001101011010
10011010100101
```

You can run the tests if you want:

```
stack test
```

You can build an executable:

```
$ stack build
$ .stack-work/dist/x86_64-linux-tinfo6/ghc-9.10.3/build/binaryPuzzle/binaryPuzzle
Usage: binaryPuzzle <filename>
```

[Prior art](https://github.com/shlomif/binary-puzzle-garden) exists for solving binary puzzles using code, but not in Haskell that I am aware of. My approach owes a lot to Richard Bird's sudoku solver in [Pearls of Functional Algortithim Design](http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/pearls-functional-algorithm-design?format=HB&isbn=9780521513388) and you can read about it in my [Binary Puzzle solver blog post](https://charlieharvey.org.uk/page/solving_binary_pussles_with_haskell).
