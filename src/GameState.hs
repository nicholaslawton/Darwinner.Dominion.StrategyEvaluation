module GameState
  ( GameState(..)
  , players
  , supply
  ) where

import Player
import PlayerWithoutDominion
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Card
import BuyAllowance

data GameState
  = New [PlayerWithoutDominion]
  | PreparingSupply [PlayerWithoutDominion] [Card]
  | PreparingDecks [PlayerPreparingStartingDeck] [Card]
  | DrawingInitialHands [PlayerDrawingInitialHand] [Card]
  | BuyPhase BuyAllowance [Player] [Card]
  | CleanUpPhase [Player] [Card]
  | GameOver
  deriving (Eq, Show)

players :: GameState -> [Player]
players (New pids) = Player.fromPlayerId <$> pids
players (PreparingSupply pids _) = Player.fromPlayerId <$> pids
players (PreparingDecks ps _) = Player.fromPlayerPreparingStartingDeck <$> ps
players (DrawingInitialHands ps _) = Player.fromPlayerDrawingInitialHand <$> ps
players (BuyPhase _ ps _) = ps
players (CleanUpPhase ps _) = ps
players GameOver = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply (DrawingInitialHands _ cards) = cards
supply (BuyPhase _ _ cards) = cards
supply (CleanUpPhase _ cards) = cards
supply GameOver = []
