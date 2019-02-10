module CommandValidation
  ( cardDrawn
  ) where

import Command
import Candidate
import Card

cardDrawn :: Command -> Maybe (CandidateId, Card)
cardDrawn (DrawCard pid card) = Just (pid, card)
cardDrawn _ = Nothing
