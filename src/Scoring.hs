module Scoring (score) where

import Player
import Card

score :: Player p => p -> Int
score = length . filter ((== Victory) . cardType) . dominion
