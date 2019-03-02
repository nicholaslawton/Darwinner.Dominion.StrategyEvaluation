module Main where

import ParsingTests
import UpdateTests
import GamePreparationTests
import BuyPhaseTests
import CleanUpPhaseTests
import TurnSequenceTests

import Test.Hspec

main :: IO ()
main = hspec $ do
  parsingTests
  updateTests
  gamePreparationTests
  buyPhaseTests
  cleanUpPhaseTests
  turnSequenceTests
