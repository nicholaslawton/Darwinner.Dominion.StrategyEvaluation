module GameDefinition
  ( GameDefinition(..)
  , Player(..)
  , PlayerId(..)
  , Strategy(..)
  , Card(..)
  , ParseError(..)
  , parse
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Text.Trifecta

data GameDefinition = GameDefinition [Player]
  deriving (Eq, Show)

data Player = Player
  { playerId :: PlayerId
  , strategy :: Strategy
  }
  deriving (Eq, Show)

data Strategy = Strategy [Card]
  deriving (Eq, Show)

data Card
  = Province
  | Duchy
  | Estate
  | Gold
  | Silver
  | Copper
  deriving (Eq, Show)

newtype PlayerId = PlayerId String
  deriving (Eq, Show)

newtype ParseError = ParseError String
  deriving (Eq, Show)

parse :: ByteString -> Either ParseError GameDefinition
parse = toEither . parseByteString parser mempty

parser :: Parser GameDefinition
parser = whiteSpace *> gameDefinition <* eof

gameDefinition :: Parser GameDefinition
gameDefinition = braces $
  field "players" $ GameDefinition <$> list player

player :: Parser Player
player = braces $
  liftA2 Player (field "id" playerIdParser) (token (char ',') *> field "strategy" strategyParser)

playerIdParser :: Parser PlayerId
playerIdParser = PlayerId <$> token (some letter)

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
literal s value = const value <$> token (string s)

toEither :: Result a -> Either ParseError a
toEither (Failure info) = Left $ ParseError $ show $ _errDoc info
toEither (Success a) = Right a
