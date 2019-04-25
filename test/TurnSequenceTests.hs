module TurnSequenceTests (turnSequenceTests) where

import Game
import GameState
import PlayState
import BuyAllowance
import CompletePlayer
import Card
import Turn

import Control.Applicative
import Data.Composition
import Data.Map hiding (mapMaybe, foldr, filter)
import Data.Maybe

import GameStateValidation
import EngineValidation
import EventValidation
import Categorisation
import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck hiding (Discard, discard)

turnSequenceTests :: SpecWith ()
turnSequenceTests = describe "turn sequence" $ do

  it "provides equal opportunity" $ property $ \params (NonEmpty deck) (NonEmpty hand) (NonEmpty discard) cids ->
    let
      pids = validCandidateIds cids
    in
      (<= 5)
      . liftA2 (-) maximum minimum
      . elems
      . fmap length
      . addEmpty pids
      . categorise fst snd
      . mapMaybe cardDrawn
      . history
      . execUntil gameOver 1000 params
      .: gameInProgress ((\pid -> CompletePlayer.new pid deck hand discard) <$> pids) []

  describe "turn end" $ do

    it "transitions to next turn when game end conditions not met" $ property $ \seed params g ->
      buyPhase
      $ state
      $ execWhile turnEnd 10 params
      $ gameInState (TurnEnd $ addProvinceToSupply g) seed

    it "transitions to game over when no province remains in supply" $ property $ \seed params g ->
      gameOver
      $ state
      $ execWhile turnEnd 10 params
      $ gameInState (TurnEnd  $ removeProvincesFromSupply g) seed

addProvinceToSupply :: PlayState -> PlayState
addProvinceToSupply g = g { PlayState.supply = Province : PlayState.supply g }

removeProvincesFromSupply :: PlayState -> PlayState
removeProvincesFromSupply g = g { PlayState.supply = filter (/= Province) $ PlayState.supply g }

addEmpty :: Ord k => [k] -> Map k [a] -> Map k [a]
addEmpty = flip $ foldr (\k m -> insertWith (flip const) k [] m)

gameInProgress :: [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInProgress = gameInState . BuyPhase BuyAllowance.initial .:. PlayState
