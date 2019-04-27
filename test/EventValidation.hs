module EventValidation
  ( playerAdded
  , cardPlacedInSupply
  , cardAddedToDeck
  , cardPutInPlay
  , cardDrawn
  , cardGained
  , cardDiscarded
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

cardDiscarded :: Event -> Maybe Card
cardDiscarded (CardDiscarded _ card) = Just card
cardDiscarded _ = Nothing
