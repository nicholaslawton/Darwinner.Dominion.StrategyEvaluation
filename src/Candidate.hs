module Candidate
  ( Candidate(..)
  ) where

import CandidateId
import Strategy

data Candidate = Candidate
  { candidateId :: CandidateId
  , strategy :: Strategy
  }
  deriving (Eq, Show)
