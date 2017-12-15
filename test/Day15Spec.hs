module Day15Spec where

import Test.Hspec
import Day15

spec :: Spec
spec = do
  describe "generator" $ do
    it "generates an endless list of values" $ do
      let generatorA = generator 16807 65
      let generatorB = generator 48271 8921

      take 5 generatorA `shouldBe` [1092455, 1181022009, 245556042, 1744312007, 1352636452]
      take 5 generatorB `shouldBe` [430625591, 1233683848, 1431495498, 137874439, 285222916]

  describe "binary streams" $ do
    it "converts an int to binary string" $ do
      let number1 = 1092455
      intToBinary number1 `shouldBe` "00000000000100001010101101100111"

      let number2 = 1744312007
      intToBinary number2 `shouldBe` "01100111111110000001011011000111"

    it "maps over a stream" $ do
      let generatorA = generator 16807 65
      let mappedGenerator = map intToBinary generatorA

      take 5 mappedGenerator `shouldBe` [ "00000000000100001010101101100111"
                                        , "01000110011001001111011100111001"
                                        , "00001110101000101110001101001010"
                                        , "01100111111110000001011011000111"
                                        , "01010000100111111001100000100100" ]

  describe "judge" $ do
    it "compares two binary strings and is contented with 16 matching endbits" $ do
      let string1 = "00000000000100001010101101100111"
      let string2 = "01000110011001001111011100111001"
      compareBits string1 string2 `shouldBe` False

      let string3 = "00001110101000101110001101001010"
      let string4 = "01010101010100101110001101001010"
      compareBits string3 string4 `shouldBe` True

    it "lets through only matching bit pairs" $ do
      let generatorA = generator 16807 65
      let generatorB = generator 48271 8921
      let unifiedStream = zip (map intToBinary generatorA) (map intToBinary generatorB)

      judge (take 5 unifiedStream) `shouldBe` [( "00001110101000101110001101001010"
                                               , "01010101010100101110001101001010" )]

--    it "solves the puzzle" $ do
--      let generatorA = generator 16807 699
--      let generatorB = generator 48271 124
--      let unifiedStream = zip (map intToBinary generatorA) (map intToBinary generatorB)
--      let amount = 40000000
--
--      length (judge (take amount unifiedStream)) `shouldBe` 600
--
--    it "solves part two of the puzzle" $ do
--      let generatorA = filter (\x -> x `mod` 4 == 0) $ generator 16807 699
--      let generatorB = filter (\x -> x `mod` 8 == 0) $ generator 48271 124
--      let unifiedStream = zip (map intToBinary generatorA) (map intToBinary generatorB)
--      let amount = 5000000
--
--      length (judge (take amount unifiedStream)) `shouldBe` 313
