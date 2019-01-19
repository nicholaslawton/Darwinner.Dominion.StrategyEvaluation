module Player
  ( Player(discard)
  , Player.new
  , fromPlayerId
  , Player.fromPlayerPreparingStartingDeck
  , fromPlayerDrawingInitialHand
  , Player.alterDeck
  , Player.alterHand
  , alterDiscard
  ) where

import GenericPlayer
import Candidate
import Card
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

fromPlayerId :: (GenericPlayer p) => p -> Player
fromPlayerId p = Player (GenericPlayer.playerId p) [] [] []

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> Player
fromPlayerPreparingStartingDeck (PlayerPreparingStartingDeck pid d) = Player pid d [] []

fromPlayerDrawingInitialHand :: PlayerDrawingInitialHand -> Player
fromPlayerDrawingInitialHand (PlayerDrawingInitialHand pid d h) = Player pid d h []

alterDeck :: ([Card] -> [Card]) -> Player -> Player
alterDeck f p = p { Player.deck = f (Player.deck p) }

alterHand :: ([Card] -> [Card]) -> Player -> Player
alterHand f p = p { Player.hand = f (Player.hand p) }

alterDiscard :: ([Card] -> [Card]) -> Player -> Player
alterDiscard f p = p { Player.discard = f (Player.discard p) }
