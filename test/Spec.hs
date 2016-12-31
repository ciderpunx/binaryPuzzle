import Test.Hspec
import BinaryPuzzle (solvedGrid, solve, Grid)
import Grids0
import Grids1
import Grids2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "identifies solved grids ok" $ do
    it "recognizes as solved completed grid 0" $
      solvedGrid completeGrid `shouldBe` True
    it "recognizes as solved completed grid 1" $
      solvedGrid testGrid `shouldBe` True
  describe "doesn't solve grids that cannot be solved" $
    it "gives empty response for unsolvable grid" $
      solve unsolvableGrid `shouldBe` []
  describe "solves completed grids" $ do
    it "solves completed grid 0" $
      solve completeGrid `shouldBe` completeGrid
    it "solves completed grid 1" $
      solve testGrid `shouldBe` testGrid
  describe "solves simple grids from the internet" $ do
    it "solves gp0" $
      solvedGrid (solve gp0) `shouldBe` True
    it "solves gp1" $
      solvedGrid (solve gp1) `shouldBe` True
    it "solves gp2" $
      solvedGrid (solve gp2) `shouldBe` True
    it "solves gp3" $
      solvedGrid (solve gp3) `shouldBe` True
    it "solves gp6" $
      solvedGrid (solve gp6) `shouldBe` True
    it "solves gp7" $
      solvedGrid (solve gp7) `shouldBe` True
    it "solves gp8" $
      solvedGrid (solve gp8) `shouldBe` True
  describe "solves 6^2 grids" $ do
    it "solves g6" $
      solvedGrid (solve g6) `shouldBe` True
    it "solves gg6" $
      solvedGrid (solve gg6) `shouldBe` True
  describe "solves 8^2 grids" $ do
    it "solves g8" $
      solvedGrid (solve g8) `shouldBe` True
    it "solves g8'" $
      solvedGrid (solve g8') `shouldBe` True
    it "solves gg8" $
      solvedGrid (solve gg8) `shouldBe` True
  describe "solves 10^2 grids" $ do
    it "solves g10" $
      solvedGrid (solve g10) `shouldBe` True
    it "solves gp3" $
      solvedGrid (solve gp3) `shouldBe` True
  describe "solves 12^2 grids" $ do
    it "solves g12" $
      solvedGrid (solve g12) `shouldBe` True
    it "solves g12'" $
      solvedGrid (solve g12') `shouldBe` True
    it "solves gg12" $
      solvedGrid (solve gg12) `shouldBe` True
  describe "solves 14^2 grids" $ do
    it "solves g14" $
      solvedGrid (solve g14) `shouldBe` True
    it "solves g14'" $
      solvedGrid (solve g14') `shouldBe` True
    it "solves gg14" $
      solvedGrid (solve gg14) `shouldBe` True
    it "solves gp4" $
      solvedGrid (solve gp4) `shouldBe` True
    it "solves gp5" $
      solvedGrid (solve gp5) `shouldBe` True
