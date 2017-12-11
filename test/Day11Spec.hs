module Day11Spec where

import Test.Hspec
import Day11

spec :: Spec
spec = do
  describe "directions from string" $ do
    it "reads directions from string 1" $ do
      directionsFromString "" `shouldBe` []

    it "reads directions from string 2" $ do
      directionsFromString "n,s,ne,nw,se,sw" `shouldBe` [North, South, NorthEast, NorthWest, SouthEast, SouthWest]

    it "reads directions from string 3" $ do
      directionsFromString "ne,ne,sw,sw" `shouldBe` [NorthEast, NorthEast, SouthWest, SouthWest]

  describe "strip opposite directions" $ do
    it "strips opposite directions 1" $ do
      let directions = []
      filterOpposites directions `shouldBe` []

    it "strips opposite directions 2" $ do
      let directions = [North, South, NorthEast, NorthWest, SouthEast, SouthWest]
      filterOpposites directions `shouldBe` []

    it "strips opposite directions 3" $ do
      let directions = [North, North, South, NorthEast, NorthEast, NorthWest, SouthEast, SouthEast, SouthWest]
      filterOpposites directions `shouldBe` [North, NorthEast, SouthEast]

    it "strips opposite directions 4" $ do
      let directions = [North, South, South, NorthEast, NorthWest, NorthWest, SouthEast, SouthWest, SouthWest]
      filterOpposites directions `shouldBe` [South, NorthWest, SouthWest]

  describe "simplify left triangles" $ do
    it "simplfy left triangles 1" $ do
      let directions = [North, SouthWest]
      simplifyLeftTriangles directions `shouldBe` [NorthWest]

    it "simplfy left triangles 2" $ do
      let directions = [NorthWest, South]
      simplifyLeftTriangles directions `shouldBe` [SouthWest]

    it "simplfy left triangles 3" $ do
      let directions = [SouthWest, SouthEast]
      simplifyLeftTriangles directions `shouldBe` [South]

    it "simplfy left triangles 4" $ do
      let directions = [South, NorthEast]
      simplifyLeftTriangles directions `shouldBe` [SouthEast]

    it "simplfy left triangles 5" $ do
      let directions = [SouthEast, North]
      simplifyLeftTriangles directions `shouldBe` [NorthEast]

    it "simplfy left triangles 6" $ do
      let directions = [NorthEast, NorthWest]
      simplifyLeftTriangles directions `shouldBe` [North]

  describe "simplify right triangles" $ do
    it "simplfy right triangles 1" $ do
      let directions = [North, SouthEast]
      simplifyRightTriangles directions `shouldBe` [NorthEast]

    it "simplfy right triangles 2" $ do
      let directions = [NorthEast, South]
      simplifyRightTriangles directions `shouldBe` [SouthEast]

    it "simplfy right triangles 3" $ do
      let directions = [SouthEast, SouthWest]
      simplifyRightTriangles directions `shouldBe` [South]

    it "simplfy right triangles 4" $ do
      let directions = [South, NorthWest]
      simplifyRightTriangles directions `shouldBe` [SouthWest]

    it "simplfy right triangles 5" $ do
      let directions = [SouthWest, North]
      simplifyRightTriangles directions `shouldBe` [NorthWest]

    it "simplfy right triangles 6" $ do
      let directions = [NorthWest, NorthEast]
      simplifyRightTriangles directions `shouldBe` [North]

  describe "part 1" $ do
    file <- runIO $ readFile "test/day11.txt"

    it "solves the puzzle" $ do
      part1 file `shouldBe` 812

--  describe "part 2" $ do
--    file <- runIO $ readFile "test/day11.txt"
--
--    it "solves the puzzle" $ do
--      part2 file `shouldBe` 1603
