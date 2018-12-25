module Candidate
  ( Candidate(..)
  , CandidateId(..)
  ) where

import Strategy

data Candidate = Candidate
  { candidateId :: CandidateId
  , strategy :: Strategy
  }
  deriving (Eq, Show)

newtype CandidateId = CandidateId String
  deriving (Eq, Ord, Show)
