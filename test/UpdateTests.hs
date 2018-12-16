module UpdateTests where

import Update
import Command
import Game

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

updateTests :: SpecWith ()
updateTests = describe "update" $ do

  describe "add player to new game" $
    it "adds player" $ property $ \ps player ->
      numPlayers (updateAction (AddPlayer player) (New ps)) == length ps + 1

numPlayers :: GameState -> Int
numPlayers (New ps) = length ps
numPlayers (PreparingSupply ps _) = length ps
numPlayers Prepared = 0
