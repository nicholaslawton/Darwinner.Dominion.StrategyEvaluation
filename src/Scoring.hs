module Scoring (score) where

import Player

score :: Player p => p -> Int
score = length . dominion
