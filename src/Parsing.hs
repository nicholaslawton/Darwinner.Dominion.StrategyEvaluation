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
parser = whiteSpace *> evaluationParameters <* eof

evaluationParameters :: Parser EvaluationParameters
evaluationParameters = braces $
  field "players" $ EvaluationParameters <$> list candidate

candidate :: Parser Candidate
candidate = braces $
  liftA2 Candidate (field "id" idParser) (token (char ',') *> field "strategy" strategyParser)

idParser :: Parser CandidateId
idParser = CandidateId <$> token (some letter)

strategyParser :: Parser Strategy
strategyParser = Strategy <$> list card

card :: Parser Card
card = choice
  [ literal "Province" Province
  , literal "Duchy" Duchy
  , literal "Estate" Estate
  , literal "Gold" Gold
  , literal "Silver" Silver
  , literal "Copper" Copper
  ]

list :: Parser a -> Parser [a]
list = brackets . commaSep

field :: String -> Parser a -> Parser a
field name p =  string name *> token (char ':') *> p

literal :: String -> a -> Parser a
literal s value = value <$ token (string s)

toEither :: Result a -> Either ParseError a
toEither (Failure info) = Left $ ParseError $ show $ _errDoc info
toEither (Success a) = Right a
