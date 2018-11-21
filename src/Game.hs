module Game
  ( Game(..)
  , GameState(..)
  , new
  ) where

import Event

import System.Random

data Game = Game
  { state :: GameState
  , history :: [Event]
  , gen :: StdGen
  }

data GameState
  = New
  | Prepared
  deriving (Eq, Show)

new :: Int -> Game
new = Game New [] . mkStdGen
