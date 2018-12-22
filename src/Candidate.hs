module Candidate where

import Strategy
import Player

data Candidate = Candidate
  { candidateId :: PlayerId
  , strategy :: Strategy
  }
  deriving (Eq, Show)
