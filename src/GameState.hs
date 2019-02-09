module GameState
  ( GameState(..)
  , CleanUpStep(..)
  , players
  , supply
  ) where

import CompletePlayer
import PlayerWithoutDominion
import PlayerWithDeck
import PlayerWithHand
import Card
import BuyAllowance

data GameState
  = New [PlayerWithoutDominion]
  | PreparingSupply [PlayerWithoutDominion] [Card]
  | PreparingDecks [PlayerWithDeck] [Card]
  | DrawingInitialHands [PlayerWithHand] [Card]
  | BuyPhase BuyAllowance [CompletePlayer] [Card]
  | CleanUpPhase CleanUpStep [CompletePlayer] [Card]
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
players (BuyPhase _ ps _) = ps
players (CleanUpPhase _ ps _) = ps
players GameOver = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply (DrawingInitialHands _ cards) = cards
supply (BuyPhase _ _ cards) = cards
supply (CleanUpPhase _ _ cards) = cards
supply GameOver = []
