module UpdateTests where

import Update
import Command
import GameState

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

updateTests :: SpecWith ()
updateTests = describe "update" $ do

  describe "add player to new game" $
    it "adds player" $ property $ \ps player ->
      length (players (update (AddPlayer player) (New ps))) == length ps + 1

  describe "players ready" $
    it "begins preparing supply" $ property $
      isPreparingSupply . update PlayersReady . New

isPreparingSupply :: GameState -> Bool
isPreparingSupply (PreparingSupply _ _) = True
isPreparingSupply _ = False
