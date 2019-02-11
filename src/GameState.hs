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
import Card
import BuyAllowance
import PlayState

data GameState
  = New [PlayerWithoutDominion]
  | PreparingSupply [PlayerWithoutDominion] [Card]
  | PreparingDecks [PlayerWithDeck] [Card]
  | DrawingInitialHands [PlayerWithHand] [Card]
  | BuyPhase PlayState BuyAllowance
  | CleanUpPhase PlayState CleanUpStep
  | GameOver
  deriving (Eq, Show)

data CleanUpStep
  = Discard
  | DrawHand
  deriving (Eq, Show)

players :: GameState -> [CompletePlayer]
players (New ps) = CompletePlayer.fromPlayerWithoutDominion <$> ps
players (PreparingSupply ps _) = CompletePlayer.fromPlayerWithoutDominion <$> ps
players (PreparingDecks ps _) = CompletePlayer.fromPlayerWithDeck <$> ps
players (DrawingInitialHands ps _) = CompletePlayer.fromPlayerWithHand <$> ps
players (BuyPhase g _) = PlayState.players g
players (CleanUpPhase g _) = PlayState.players g
players GameOver = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply (DrawingInitialHands _ cards) = cards
supply (BuyPhase g _) = PlayState.supply g
supply (CleanUpPhase g _) = PlayState.supply g
supply GameOver = []
