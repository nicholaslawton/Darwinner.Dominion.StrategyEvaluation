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
  | BuyPhase BuyAllowance PlayState
  | CleanUpPhase CleanUpStep PlayState
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
players (BuyPhase _ g) = PlayState.players g
players (CleanUpPhase _ g) = PlayState.players g
players GameOver = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply (DrawingInitialHands _ cards) = cards
supply (BuyPhase _ g) = PlayState.supply g
supply (CleanUpPhase _ g) = PlayState.supply g
supply GameOver = []
