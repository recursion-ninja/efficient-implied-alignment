module Main
  ( main
  ) where

import qualified Alignment.Pairwise.Test as Pairwise
import           Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "Alignment Test Suite"
    [ Pairwise.testSuite
    ]

