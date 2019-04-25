module CommandValidation
  ( playerAdded
  , cardPlacedInSupply
  , cardAddedToDeck
  , cardPutInPlay
  , cardDrawn
  , cardGained
  , cardDiscarded
  ) where

import Command
import Candidate
import Card

import Control.Applicative

playerAdded :: Command -> Maybe CandidateId
playerAdded (PlayerAdded pid) = Just pid
playerAdded _ = Nothing

cardPlacedInSupply :: Command -> Maybe Card
cardPlacedInSupply (CardPlacedInSupply card) = Just card
cardPlacedInSupply _ = Nothing

cardAddedToDeck :: Command -> Maybe (CandidateId, Card)
cardAddedToDeck (CardAddedToDeck pid card) = Just (pid, card)
cardAddedToDeck _ = Nothing

cardPutInPlay :: Command -> Maybe Card
cardPutInPlay = liftA2 (<|>) cardPlacedInSupply (fmap snd . cardAddedToDeck)

cardDrawn :: Command -> Maybe (CandidateId, Card)
cardDrawn (CardDrawn pid card) = Just (pid, card)
cardDrawn _ = Nothing

cardGained :: Command -> Bool
cardGained (CardGained _ _) = True
cardGained _ = False

cardDiscarded :: Command -> Maybe Card
cardDiscarded (CardDiscarded _ card) = Just card
cardDiscarded _ = Nothing
