module Day10Spec where

import Test.Hspec
import Day10

spec :: Spec
spec = do
  describe "knot" $ do
    it "makes a knot 1" $ do
      let state = State { circle = [0..4]
                        , position = 0
                        , skipSize = 0 }
      knot state 3 `shouldBe` State { circle = [2, 1, 0, 3, 4], position = 3, skipSize = 1 }

    it "makes a knot 2" $ do
      let state = State { circle = [2, 1, 0, 3, 4]
                        , position = 3
                        , skipSize = 1 }
      knot state 4 `shouldBe` State { circle = [4, 3, 0, 1, 2], position = 3, skipSize = 2 }

    it "makes a knot 3" $ do
      let state = State { circle = [4, 3, 0, 1, 2]
                        , position = 3
                        , skipSize = 2 }
      knot state 1 `shouldBe` State { circle = [4, 3, 0, 1, 2], position = 1, skipSize = 3 }

    it "makes a knot 4" $ do
      let state = State { circle = [4, 3, 0, 1, 2]
                        , position = 1
                        , skipSize = 3 }
      knot state 5 `shouldBe` State { circle = [3, 4, 2, 1, 0], position = 4, skipSize = 4 }

    it "solves the puzzle" $ do
      let state = State { circle = [0..255]
                        , position = 0
                        , skipSize = 0 }
      let lengths = [225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110]
      let lotsOfKnots = foldl knot state lengths
      checksum lotsOfKnots `shouldBe` 23874
