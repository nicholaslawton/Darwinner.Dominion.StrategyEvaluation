module GameState
  ( GameState(..)
  , players
  , supply
  ) where

import Candidate
import Player
import PlayerPreparingStartingDeck
import Card

data GameState
  = New [CandidateId]
  | PreparingSupply [CandidateId] [Card]
  | PreparingDecks [PlayerPreparingStartingDeck] [Card]
  | DrawingInitialHands [Player] [Card]
  | Prepared
  deriving (Eq, Show)

players :: GameState -> [Player]
players (New pids) = Player.new <$> pids
players (PreparingSupply pids _) = Player.new <$> pids
players (PreparingDecks ps _) = Player.fromPlayerPreparingStartingDeck <$> ps
players (DrawingInitialHands ps _) = ps
players Prepared = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply (DrawingInitialHands _ cards) = cards
supply Prepared = []
