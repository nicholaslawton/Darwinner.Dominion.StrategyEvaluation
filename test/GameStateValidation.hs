module GameStateValidation
  ( preparingSupply
  , preparingDecks
  , drawingInitialHands
  , inCleanUpPhase
  , inBuyPhase )
  where

import GameState

preparingSupply :: GameState -> Bool
preparingSupply (PreparingSupply _ _) = True
preparingSupply _ = False

preparingDecks :: GameState -> Bool
preparingDecks (PreparingDecks _ _) = True
preparingDecks _ = False

drawingInitialHands :: GameState -> Bool
drawingInitialHands (DrawingInitialHands _ _) = True
drawingInitialHands _ = False

inCleanUpPhase :: GameState -> Bool
inCleanUpPhase (CleanUpPhase _ _) = True
inCleanUpPhase _ = False

inBuyPhase :: GameState -> Bool
inBuyPhase (BuyPhase _ _ _) = True
inBuyPhase _ = False
