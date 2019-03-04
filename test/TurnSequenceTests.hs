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
import Data.Map hiding (mapMaybe)
import Data.Maybe

import GameStateValidation
import EngineValidation
import CommandValidation
import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck hiding (Discard, discard)

turnSequenceTests :: SpecWith ()
turnSequenceTests = describe "turn sequence" $ do

  it "does not end immediately" $ property $ \params (ValidPlayers ps) ->
    not . gameOver . state . execUntil gameOver 1000 params .:. gameInProgress ps

  it "provides equal opportunity" $ property $ \params (NonEmpty deck) (NonEmpty hand) (NonEmpty discard) cids ->
    let
      pids = validCandidateIds cids
    in
      (<= 5)
      . liftA2 (-) maximum minimum
      . elems
      . fmap length
      . categorise fst snd
      . mapMaybe cardDrawn
      . history
      . execUntil gameOver 1000 params
      .:. gameInProgress ((\pid -> CompletePlayer.new pid deck hand discard) <$> pids)

categorise :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
categorise key value xs = fromListWith (++) $ liftA2 (,) key ((: []) . value) <$> xs

gameInProgress :: [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInProgress = gameInState . BuyPhase BuyAllowance.initial .:. PlayState
