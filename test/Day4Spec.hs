module Day4Spec where

import Test.Hspec
import Day4

spec :: Spec
spec = do
  describe "checkPassphrases1" $ do
    it "finds a faulty passphrase" $ do
      let passphrases = [["foo", "bar", "foo"]]
      checkPassphrases1 passphrases `shouldBe` []

    it "succeeds with all correct passphrases" $ do
      let passphrases = [["foo", "bar", "baz"], ["uno", "dos", "tres"], ["ram", "sam"]]
      checkPassphrases1 passphrases `shouldBe` passphrases

    it "works with demo input" $ do
      let passphrases = [ ["aa", "bb", "cc", "dd", "ee"]
                        , ["aa", "bb", "cc", "dd", "aa"]
                        , ["aa", "bb", "cc", "dd", "aaa"] ]
      let expectedOutcome = [ ["aa", "bb", "cc", "dd", "ee"]
                            , ["aa", "bb", "cc", "dd", "aaa"] ]
      checkPassphrases1 passphrases `shouldBe` expectedOutcome

  describe "solution 1 from file" $ do
    file <- runIO $ readFile "test/day4.txt"

    it "solves the puzzle" $ do
      let passphrases = map words $ lines file
      length (checkPassphrases1 passphrases) `shouldBe` 337

  describe "checkPassphrases2" $ do
    it "finds a faulty passphrase" $ do
      let passphrases = [["foo", "bar", "oof"]]
      checkPassphrases2 passphrases `shouldBe` []

    it "works with demo input" $ do
      let passphrases = [ ["abcde", "fghij"]
                        , ["abcde", "xyz", "ecdab"]
                        , ["a", "ab", "abc", "abd", "abf", "abj"]
                        , ["iiii", "oiii", "ooii", "oooi", "oooo"]
                        , ["oiii", "ioii", "iioi", "iiio"] ]
      let expected = [ ["abcde", "fghij"]
                     , ["a", "ab", "abc", "abd", "abf", "abj"]
                     , ["iiii", "oiii", "ooii", "oooi", "oooo"] ]
      checkPassphrases2 passphrases `shouldBe` expected

  describe "solution 2 from file" $ do
    file <- runIO $ readFile "test/day4.txt"

    it "solves the puzzle" $ do
      let passphrases = map words $ lines file
      length (checkPassphrases2 passphrases) `shouldBe` 231
