module Main where

import ParsingTests
import GamePreparationTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  parsingTests
  gamePreparationTests
