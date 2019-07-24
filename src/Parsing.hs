module Parsing
  ( ParseError(..)
  , parseEvaluationParameters
  ) where

import Card (Card(..))
import Strategy
import Candidate
import CandidateId
import EvaluationParameters

import Control.Applicative
import Data.ByteString (ByteString)
import Data.List
import Text.Trifecta

newtype ParseError = ParseError String
  deriving (Eq, Show)

parseEvaluationParameters :: ByteString -> Either ParseError EvaluationParameters
parseEvaluationParameters s = parse s >>= validatePlayerCount >>= validateCandidateIdentifiers

parse :: ByteString -> Either ParseError EvaluationParameters
parse = toEither . parseByteString parser mempty

validatePlayerCount :: EvaluationParameters -> Either ParseError EvaluationParameters
validatePlayerCount (EvaluationParameters candidates) =
  case length candidates of
    x | x < 2 -> Left $ ParseError "Insufficient number of players (minimum two)"
    x | x > 4 -> Left $ ParseError "Too many players (maximum four)"
    _         -> Right $ EvaluationParameters candidates

validateCandidateIdentifiers :: EvaluationParameters -> Either ParseError EvaluationParameters
validateCandidateIdentifiers (EvaluationParameters candidates) = 
  if containsDuplicates $ candidateId <$> candidates
  then Left $ ParseError "Duplicate candidate identifiers"
  else Right $ EvaluationParameters candidates
    where
      containsDuplicates = liftA2 (/=) length (length . nub)

parser :: Parser EvaluationParameters
parser = evaluationParameters <* eof

evaluationParameters :: Parser EvaluationParameters
evaluationParameters = EvaluationParameters <$> some (whiteSpace *> candidate)

candidate :: Parser Candidate
candidate = liftA2 Candidate idParser (token (char ':') *> strategyParser)

idParser :: Parser CandidateId
idParser = CandidateId <$> token (some letter)

strategyParser :: Parser Strategy
strategyParser = Strategy <$> commaSep card

card :: Parser Card
card = choice
  [ literal "Province" Province
  , literal "Duchy" Duchy
  , literal "Estate" Estate
  , literal "Gold" Gold
  , literal "Silver" Silver
  , literal "Copper" Copper
  ]

literal :: String -> a -> Parser a
literal s value = value <$ token (string s)

toEither :: Result a -> Either ParseError a
toEither (Failure info) = Left $ ParseError $ show $ _errDoc info
toEither (Success a) = Right a
