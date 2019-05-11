module EventValidation
  ( playerAdded
  , cardPlacedInSupply
  , cardAddedToDeck
  , cardPutInPlay
  , cardDrawn
  , cardGained
  , cardPlayed
  , unplayedCardDiscarded
  , playedCardDiscarded
  ) where

import Event
import CandidateId
import Card

import Control.Applicative

playerAdded :: Event -> Maybe CandidateId
playerAdded (PlayerAdded pid) = Just pid
playerAdded _ = Nothing

cardPlacedInSupply :: Event -> Maybe Card
cardPlacedInSupply (CardPlacedInSupply card) = Just card
cardPlacedInSupply _ = Nothing

cardAddedToDeck :: Event -> Maybe (CandidateId, Card)
cardAddedToDeck (CardAddedToDeck pid card) = Just (pid, card)
cardAddedToDeck _ = Nothing

cardPutInPlay :: Event -> Maybe Card
cardPutInPlay = liftA2 (<|>) cardPlacedInSupply (fmap snd . cardAddedToDeck)

cardDrawn :: Event -> Maybe (CandidateId, Card)
cardDrawn (CardDrawn pid card) = Just (pid, card)
cardDrawn _ = Nothing

cardGained :: Event -> Bool
cardGained (CardGained _ _) = True
cardGained _ = False

cardPlayed :: Event -> Maybe Card
cardPlayed (TreasureCardPlayed _ card) = Just card
cardPlayed _ = Nothing

unplayedCardDiscarded :: Event -> Maybe Card
unplayedCardDiscarded (UnplayedCardDiscarded _ card) = Just card
unplayedCardDiscarded _ = Nothing

playedCardDiscarded :: Event -> Maybe Card
playedCardDiscarded (PlayedCardDiscarded _ card) = Just card
playedCardDiscarded _ = Nothing
