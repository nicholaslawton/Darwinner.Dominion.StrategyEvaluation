module Candidate
  ( Candidate(..)
  , PlayerId(..)
  ) where

import Strategy

data Candidate = Candidate
  { candidateId :: PlayerId
  , strategy :: Strategy
  }
  deriving (Eq, Show)

newtype PlayerId = PlayerId String
  deriving (Eq, Ord, Show)
