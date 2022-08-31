{-# Language ImportQualifiedPost #-}

module Main
    ( main
    , testSuite
    ) where

import Alignment.Pairwise.Test qualified as Pairwise
import Test.Tasty


main :: IO ()
main = testSuite


testSuite :: IO ()
testSuite = defaultMain $ testGroup "Alignment Test Suite" [Pairwise.testSuite]

