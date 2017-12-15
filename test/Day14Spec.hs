module Day14Spec where

import Test.Hspec
import Day14

spec :: Spec
spec = do
--  describe "ones in grid" $ do
--    it "counts ones in the grid" $ do
--      let grid = gridKnotHash "flqrgnkx" 128
--      let bingrid = map hexStringToBinString grid
--      onesInGrid bingrid `shouldBe` 8108
--
--    it "solves the puzzle" $ do
--      let grid = gridKnotHash "ugkiagan" 128
--      let bingrid = map hexStringToBinString grid
--      onesInGrid bingrid `shouldBe` 8292

  describe "regions in grid" $ do
    it "detects regions in a small grid 1" $ do
      let grid = [ "1010"
                 , "0101"
                 , "1010"
                 , "0101" ]

      regionsInGrid grid `shouldBe` [ [(1,1)], [(1,3)], [(2,2)], [(2,4)]
                                    , [(3,1)], [(3,3)], [(4,2)], [(4,4)] ]

    it "detects regions in a small grid 2" $ do
      let grid = [ "1011"
                 , "1101"
                 , "1010"
                 , "0111" ]

      regionsInGrid grid `shouldBe` [ [(3,1),(2,2),(2,1),(1,1)]
                                    , [(2,4),(1,4),(1,3)]
                                    , [(4,4),(4,3), (3,3),(4,2)] ]

--    it "solves the puzzle" $ do
--      let grid = gridKnotHash "ugkiagan" 128
--      let bingrid = map hexStringToBinString grid
--      length (regionsInGrid bingrid) `shouldBe` 1069
