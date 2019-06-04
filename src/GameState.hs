module GameState
  ( GameState(..)
  , CleanUpStep(..)
  , GameState.players
  , GameState.supply
  ) where

import CompletePlayer
import PlayerWithoutDominion
import PlayerWithDeck
import PlayerWithHand
import Supply
import Coins
import BuyAllowance
import PlayState

data GameState
  = New [PlayerWithoutDominion]
  | PreparingDecks [PlayerWithDeck]
  | DrawingInitialHands [PlayerWithHand]
  | PreparingSupply [CompletePlayer] Supply
  | BuyPhase Coins BuyAllowance PlayState
  | CleanUpPhase CleanUpStep PlayState
  | TurnEnd PlayState
  | GameOver
  deriving (Eq, Show)

data CleanUpStep
  = Discard
  | DrawHand
  deriving (Eq, Show)

players :: GameState -> [CompletePlayer]
players (New ps) = CompletePlayer.fromPlayerWithoutDominion <$> ps
players (PreparingDecks ps) = CompletePlayer.fromPlayerWithDeck <$> ps
players (DrawingInitialHands ps) = CompletePlayer.fromPlayerWithHand <$> ps
players (PreparingSupply ps _) = ps
players (BuyPhase _ _ g) = PlayState.players g
players (CleanUpPhase _ g) = PlayState.players g
players (TurnEnd g) = PlayState.players g
players GameOver = []

supply :: GameState -> Supply
supply (New _) = Supply.empty
supply (PreparingDecks _) = Supply.empty
supply (DrawingInitialHands _) = Supply.empty
supply (PreparingSupply _ s) = s
supply (BuyPhase _ _ g) = PlayState.supply g
supply (CleanUpPhase _ g) = PlayState.supply g
supply (TurnEnd g) = PlayState.supply g
supply GameOver = Supply.empty
