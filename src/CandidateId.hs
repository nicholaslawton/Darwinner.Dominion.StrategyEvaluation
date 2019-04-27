module CandidateId (CandidateId(..)) where

newtype CandidateId = CandidateId String
  deriving (Eq, Ord, Show)
