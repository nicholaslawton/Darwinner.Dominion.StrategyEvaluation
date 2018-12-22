module EvaluationParameters
  (EvaluationParameters(..)) where

import Candidate

data EvaluationParameters = EvaluationParameters [Candidate]
  deriving (Eq, Show)
