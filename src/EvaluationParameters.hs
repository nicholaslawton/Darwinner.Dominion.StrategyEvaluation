module EvaluationParameters
  (EvaluationParameters(..)) where

import Player

data EvaluationParameters = EvaluationParameters [Player]
  deriving (Eq, Show)