module GameStateValidation
  ( preparingSupply
  , preparingDecks
  , drawingInitialHands
  , cleanUpPhase
  , buyPhase
  , gameInState
  ) where

import GameState
import Game

preparingSupply :: GameState -> Bool
preparingSupply (PreparingSupply _ _) = True
preparingSupply _ = False

preparingDecks :: GameState -> Bool
preparingDecks (PreparingDecks _ _) = True
preparingDecks _ = False

drawingInitialHands :: GameState -> Bool
drawingInitialHands (DrawingInitialHands _ _) = True
drawingInitialHands _ = False

cleanUpPhase :: GameState -> Bool
cleanUpPhase (CleanUpPhase _ _ _) = True
cleanUpPhase _ = False

buyPhase :: GameState -> Bool
buyPhase (BuyPhase _ _ _) = True
buyPhase _ = False

gameInState :: GameState -> Int -> Game
gameInState gameState = Game.mapState (const gameState) . Game.new
