module Main where

import ParsingTests
import UpdateTests
import GamePreparationTests
import BuyPhaseTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  parsingTests
  updateTests
  gamePreparationTests
  buyPhaseTests
