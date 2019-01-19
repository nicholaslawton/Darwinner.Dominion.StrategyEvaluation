module PlayerWithoutDominion (PlayerWithoutDominion(..)) where

import GenericPlayer
import Candidate

data PlayerWithoutDominion = PlayerWithoutDominion CandidateId
  deriving (Eq, Show)

instance GenericPlayer PlayerWithoutDominion where
  playerId (PlayerWithoutDominion pid) = pid
  deck = const []
  hand = const []
  discard = const []
