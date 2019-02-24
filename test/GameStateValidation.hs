module GameStateValidation
  ( preparingSupply
  , preparingDecks
  , drawingInitialHands
  , buyPhase
  , cleanUpPhase
  , drawHandStep
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

buyPhase :: GameState -> Bool
buyPhase (BuyPhase _ _) = True
buyPhase _ = False

cleanUpPhase :: GameState -> Bool
cleanUpPhase (CleanUpPhase _ _) = True
cleanUpPhase _ = False

drawHandStep :: GameState -> Bool
drawHandStep (CleanUpPhase DrawHand _) = True
drawHandStep _ = False

gameInState :: GameState -> Int -> Game
gameInState gameState = Game.mapState (const gameState) . Game.new
