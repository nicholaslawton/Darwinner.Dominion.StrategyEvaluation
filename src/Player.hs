module Player
  ( Player
  , Player.new
  , Player.fromPlayerWithoutDominion
  , Player.fromPlayerPreparingStartingDeck
  , fromPlayerDrawingInitialHand
  , Player.alterDeck
  , Player.alterHand
  , alterDiscard
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerWithoutDominion
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand

data Player = Player
  { playerId :: CandidateId
  , deck :: [Card]
  , hand :: [Card]
  , discard :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer Player where
  playerId = Player.playerId
  deck = Player.deck
  hand = Player.hand
  discard = Player.discard

new :: CandidateId -> [Card] -> [Card] -> [Card] -> Player
new = Player 

fromPlayerWithoutDominion :: PlayerWithoutDominion -> Player
fromPlayerWithoutDominion p = Player (GenericPlayer.playerId p) [] [] []

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> Player
fromPlayerPreparingStartingDeck p = Player (GenericPlayer.playerId p) (GenericPlayer.deck p) [] []

fromPlayerDrawingInitialHand :: PlayerDrawingInitialHand -> Player
fromPlayerDrawingInitialHand p = Player (GenericPlayer.playerId p) (GenericPlayer.deck p) (GenericPlayer.hand p) []

alterDeck :: ([Card] -> [Card]) -> Player -> Player
alterDeck f p = p { Player.deck = f (Player.deck p) }

alterHand :: ([Card] -> [Card]) -> Player -> Player
alterHand f p = p { Player.hand = f (Player.hand p) }

alterDiscard :: ([Card] -> [Card]) -> Player -> Player
alterDiscard f p = p { Player.discard = f (Player.discard p) }
