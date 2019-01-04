module GameState
  ( GameState(..)
  , players
  , supply
  ) where

import Candidate
import Player
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Card

data GameState
  = New [CandidateId]
  | PreparingSupply [CandidateId] [Card]
  | PreparingDecks [PlayerPreparingStartingDeck] [Card]
  | DrawingInitialHands [PlayerDrawingInitialHand] [Card]
  | InProgress [Player] [Card]
  | GameOver
  deriving (Eq, Show)

players :: GameState -> [Player]
players (New pids) = Player.fromPlayerId <$> pids
players (PreparingSupply pids _) = Player.fromPlayerId <$> pids
players (PreparingDecks ps _) = Player.fromPlayerPreparingStartingDeck <$> ps
players (DrawingInitialHands ps _) = Player.fromPlayerDrawingInitialHand <$> ps
players (InProgress ps _) = ps
players GameOver = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply (DrawingInitialHands _ cards) = cards
supply (InProgress _ cards) = cards
supply GameOver = []
