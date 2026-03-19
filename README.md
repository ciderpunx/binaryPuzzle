This is a Haskell program to solve binary puzzles, or binary sudoku like those you can find at [binarypuzzle.com](http://www.binarypuzzle.com/index.php).

## Run on Debian

You need stack installed

```
# app install haskell-stack
```

Then git clone:

```
git clone https://github.com/ciderpunx/binaryPuzzle.git
```

Then run with stack. The first run will install many things.

```
$ stack run
01100101001011
00101010110101
11011010100100
00110101011010
11001001100101
00110110101100
10010110010011
11001001100110
01100101011001
10011010011010
10011001100110
01100110011001
11001100110010
00110011001101
```

The solved Binary Puzzle is from a Dutch puzzle book that I came across. You have to input puzzles manually in app/Main (which is quite annoying) I will maybe make a nice way to input puzzles one day.

You can run the tests if you want:

```
stack test
```

[Prior art](https://github.com/shlomif/binary-puzzle-garden) exists for solving binary puzzles using code, but not in Haskell that I am aware of. My approach owes a lot to Richard Bird's sudoku solver in [Pearls of Functional Algortithim Design](http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/pearls-functional-algorithm-design?format=HB&isbn=9780521513388) and you can read about it in my blog post at charlieharvey.org.uk/page/solving_binary_pussles_with_haskell.
