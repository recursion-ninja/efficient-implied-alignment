module Main
    ( main
    ) where

import Alignment.Pairwise.Test qualified as Pairwise
import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "Alignment Test Suite" [Pairwise.testSuite]

