module Day13Spec where

import Test.Hspec
import Day13

spec :: Spec
spec = do
  describe "caught on level" $ do
    it "does not catch anyone on a level without scanner" $ do
      caughtOnLevel 0 0 `shouldBe` False

    it "checks if we're caught on a given level 1" $ do
      caughtOnLevel 0 3 `shouldBe` True

    it "checks if we're caught on a given level 2" $ do
      caughtOnLevel 1 2 `shouldBe` False

    it "checks if we're caught on a given level 3" $ do
      caughtOnLevel 4 4 `shouldBe` False

    it "checks if we're caught on a given level 4" $ do
      caughtOnLevel 6 4 `shouldBe` True

  describe "severity" $ do
    it "calculates the severity of a trip through a firewall" $ do
      let firewall = [(0, 3), (1, 2), (4, 4), (6, 4)]
      severity firewall `shouldBe` 24

    file <- runIO $ readFile "test/day13.txt"
    it "solves the puzzle" $ do
      let firewall = firewallFromFile file
      severity firewall `shouldBe` 1876

  describe "wait before going" $ do
    it "calculates how many picoseconds to wait before going without getting caught" $ do
      let firewall = [(0, 3), (1, 2), (4, 4), (6, 4)]
      waitBeforeGoing firewall `shouldBe` 10

    file <- runIO $ readFile "test/day13.txt"
    it "solves the puzzle" $ do
      let firewall = firewallFromFile file
      waitBeforeGoing firewall `shouldBe` 3964778
