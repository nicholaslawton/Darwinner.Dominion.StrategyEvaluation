module GenericPlayer where

import Candidate

class GenericPlayer a where
  playerId :: a -> CandidateId
