module Day4 where

import Data.List
import Data.List.Unique

type Passphrase = [String]

validPassphrase :: Passphrase -> Bool
validPassphrase = allUnique

checkPassphrases1 :: [Passphrase] -> [Passphrase]
checkPassphrases1 = filter validPassphrase

checkPassphrases2 :: [Passphrase] -> [Passphrase]
checkPassphrases2 = filter (validPassphrase . map sort)
