module TurnSequenceTests (turnSequenceTests) where

import Game
import GameState
import PlayState
import Coins
import BuyAllowance
import CompletePlayer
import Card
import Supply
import Turn
import EvaluationParameters
import Candidate

import Control.Applicative
import Data.Composition
import Data.Map hiding (mapMaybe, foldr, filter)
import Data.Maybe

import GameStateValidation
import EngineValidation
import EventValidation
import Categorisation
import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck hiding (Discard, discard)

turnSequenceTests :: SpecWith ()
turnSequenceTests = describe "turn sequence" $ do

  it "provides equal opportunity" $ property $
    \params@(EvaluationParameters candidates) (NonEmpty deck) (NonEmpty hand) (NonEmpty discard) ->
      let
        pids = candidateId <$> candidates
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
        .: gameInProgress ((\pid -> CompletePlayer.new pid deck hand [] discard) <$> pids) Supply.empty

  describe "turn end" $ do

    it "transitions to next turn when game end conditions not met" $ property $ \params ->
      buyPhase . state . execWhile turnEnd 10 params .: gameAtTurnEnd (alterSupply $ Supply.add Province)

    it "transitions to game over when no province remains in supply" $ property $ \params ->
      gameOver . state . execWhile turnEnd 10 params .: gameAtTurnEnd (alterSupply $ removeAll Province)

alterSupply :: (Supply -> Supply) -> PlayState -> PlayState
alterSupply f g = g { PlayState.supply = f $ PlayState.supply g }

removeAll :: Card -> Supply -> Supply
removeAll card = until ((<= 0) . pileSize card) (Supply.remove card)

addEmpty :: Ord k => [k] -> Map k [a] -> Map k [a]
addEmpty = flip $ foldr (\k m -> insertWith (flip const) k [] m)

gameInProgress :: [CompletePlayer] -> Supply -> Turn -> Int -> Game
gameInProgress = gameInState . BuyPhase Coins.base BuyAllowance.initial .:. PlayState

gameAtTurnEnd :: (PlayState -> PlayState) -> PlayState -> Int -> Game
gameAtTurnEnd f = gameInState . TurnEnd . f
