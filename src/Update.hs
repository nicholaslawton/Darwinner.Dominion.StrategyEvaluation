module Update (update) where

import CandidateId
import Message
import GameState hiding (players, supply)
import PlayState
import Player
import PlayerWithoutDominion
import PlayerWithDeck
import PlayerWithHand
import CompletePlayer
import Card
import Coins
import BuyAllowance
import Turn

import Data.Bool
import Data.List
import Control.Applicative

update :: Message -> GameState -> GameState
update (AddPlayer pid) = addPlayer pid
update MarkPlayersReady = beginPreparingSupply
update (PlaceCardInSupply card) = placeCardInSupply card
update MarkSupplyPrepared = beginPreparingDecks
update (AddCardToDeck pid card) = addCardToDeck pid card
update MarkDecksPrepared = beginDrawingInitialHands
update MarkInitialHandsDrawn = beginPlay
update (DrawCard pid card) = drawCard pid card
update (GainCard pid card) = gainCard pid card
update (PlayTreasureCard pid card) = playTreasureCard pid card
update (DiscardCard pid card) = discardCard pid card
update (DiscardPlayedCard pid card) = discardPlayedCard pid card
update (ReformDeck pid) = reformDeck pid
update BuyPhaseComplete = beginCleanUpPhase
update DiscardStepComplete = beginDrawingNextHand
update DrawHandStepComplete = advanceToTurnEnd
update EndTurn = startNextTurn
update EndGame = endGame

addPlayer :: CandidateId -> GameState -> GameState
addPlayer pid (New ps)
  | any ((==) pid . playerId) ps = error "Invalid player join: player already in game"
  | otherwise = New $ PlayerWithoutDominion.new pid : ps
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New pids) = PreparingSupply pids []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply pids cards) = PreparingSupply pids $ card : cards
placeCardInSupply _ _ = error "A card may only be placed in the supply during game preparation"

beginPreparingDecks :: GameState -> GameState
beginPreparingDecks (PreparingSupply pids cards) =
  PreparingDecks (PlayerWithDeck.fromPlayerWithoutDominion <$> pids) cards
beginPreparingDecks _ = error "Deck preparation should occur after the supply has been prepared"

addCardToDeck :: CandidateId -> Card -> GameState -> GameState
addCardToDeck pid card (PreparingDecks ps cards)
  | all ((/=) pid . playerId) ps = error "Invalid deck preparation: player not in game"
  | otherwise = PreparingDecks (alterPlayer (alterDeck (card :)) pid ps) cards
addCardToDeck _ _ _ = error "A card may only be added to a deck during game preparation"

beginDrawingInitialHands :: GameState -> GameState
beginDrawingInitialHands (PreparingDecks ps cards) =
  DrawingInitialHands (PlayerWithHand.fromPlayerWithDeck <$> ps) cards
beginDrawingInitialHands _ = error "Drawing initial hands must occur after decks have been prepared"

beginPlay :: GameState -> GameState
beginPlay (DrawingInitialHands ps cards) =
  BuyPhase Coins.base BuyAllowance.initial (PlayState (CompletePlayer.fromPlayerWithHand <$> ps) cards firstTurn)
beginPlay _ = error "Cannot begin play before the game has been fully prepared"

drawCard :: CandidateId -> Card -> GameState -> GameState
drawCard pid card (DrawingInitialHands ps cards) = DrawingInitialHands (drawCardForPlayer pid card ps) cards
drawCard pid card (CleanUpPhase DrawHand g) =
  CleanUpPhase DrawHand $ g { players = drawCardForPlayer pid card (players g) }
drawCard _ _ _ =
  error "A card may only be drawn while players are drawing their initial hands, or during the clean up phase"

drawCardForPlayer :: Player p => CandidateId -> Card -> [p] -> [p]
drawCardForPlayer pid card ps
  | all ((/=) pid . playerId) ps = error "Invalid card draw: player not in game"
  | not $ cardBelongsToPlayer deck card pid ps = error "Invalid card draw: card not in deck of player"
  | otherwise = alterPlayer (alterHand (card :) . alterDeck (delete card)) pid ps

gainCard :: CandidateId -> Card -> GameState -> GameState
gainCard pid card (BuyPhase coins (BuyAllowance buys) g)
  | not $ playerExists pid ps = error "Invalid card gain: player not in game"
  | notElem card cards = error "Invalid card gain: card not in supply"
  | buys <= 0 = error "Invalid card gain: buy allowance exhausted"
  | otherwise = 
      BuyPhase
        coins
        (BuyAllowance (buys - 1))
        (g
          { players = alterPlayer (alterDiscard (card :)) pid ps
          , supply = delete card cards
          })
      where
        ps = players g
        cards = supply g
gainCard _ _ _ = error "A card may only be gained during the buy phase"

playTreasureCard :: CandidateId -> Card -> GameState -> GameState
playTreasureCard pid card (BuyPhase coins buys g)
  | not $ playerExists pid ps = error "Invalid treausre card play: player not in game"
  | not $ cardBelongsToPlayer hand card pid ps = error "Invalid treasure card play: card not in hand of player"
  | otherwise = BuyPhase (coins + value card) buys $ alterPlayerInState (playCard card) pid g
      where
        ps = players g
playTreasureCard _ _ _ = error "A treasure card may only be played during the buy phase"

discardCard :: CandidateId -> Card -> GameState -> GameState
discardCard pid card (CleanUpPhase Discard g)
  | not $ playerExists pid ps = error "Invalid discard: player not in game"
  | not $ cardBelongsToPlayer hand card pid ps = error "Invalid discard: card not in hand of player"
  | otherwise = CleanUpPhase Discard $ alterPlayerInState (moveFromHandToDiscard card) pid g
      where
        ps = players g
discardCard _ _ _ = error "A card may only be discarded during the discard step of the clean up phase"

discardPlayedCard :: CandidateId -> Card -> GameState -> GameState
discardPlayedCard pid card (CleanUpPhase Discard g)
  | not $ playerExists pid ps = error "Invalid discard of played card: player not in game"
  | not $ cardBelongsToPlayer playedCards card pid ps = error "Invalid discard of played card: card not played by player"
  | otherwise = CleanUpPhase Discard $ alterPlayerInState (moveFromPlayedCardsToDiscard card) pid g
      where
        ps = players g
discardPlayedCard _ _ _ = error "A played card may only be discarded during the discard step of the clean up phase"

reformDeck :: CandidateId -> GameState -> GameState
reformDeck pid (CleanUpPhase DrawHand g)
  | not $ playerExists pid ps = error "Invalid deck reformation: player not in game"
  | otherwise = CleanUpPhase DrawHand $ alterPlayerInState moveDiscardToDeck pid g
      where
        ps = players g
reformDeck _ _ = error "Reforming the deck must occur while drawing the next hand during the clean up phase"

beginCleanUpPhase :: GameState -> GameState
beginCleanUpPhase (BuyPhase _ _ g) = CleanUpPhase Discard g
beginCleanUpPhase _ = error "Clean up phase must follow buy phase"

beginDrawingNextHand :: GameState -> GameState
beginDrawingNextHand (CleanUpPhase Discard g) = CleanUpPhase DrawHand g
beginDrawingNextHand _ = error "Drawing the next hand must follow the discard step of the clean up phase"

advanceToTurnEnd :: GameState -> GameState
advanceToTurnEnd (CleanUpPhase DrawHand g) = TurnEnd g
advanceToTurnEnd _ = error "Cannot advance to turn end before clean up phase is complete"

startNextTurn :: GameState -> GameState
startNextTurn (TurnEnd g) =
  BuyPhase Coins.base BuyAllowance.initial $ g { turn = nextTurn (turn g) }
startNextTurn _ = error "Cannot start next turn before current turn is complete"

endGame :: GameState -> GameState
endGame = const GameOver

playCard :: Card -> CompletePlayer -> CompletePlayer
playCard card = alterPlayedCards (Copper :) . alterHand (delete card)

moveFromHandToDiscard :: Card -> CompletePlayer -> CompletePlayer
moveFromHandToDiscard card = alterDiscard (card :) . alterHand (delete card)

moveFromPlayedCardsToDiscard :: Card -> CompletePlayer -> CompletePlayer
moveFromPlayedCardsToDiscard card = alterDiscard (Copper :) . alterPlayedCards (delete card)

moveDiscardToDeck :: CompletePlayer -> CompletePlayer
moveDiscardToDeck p = alterDiscard (const []) $ alterDeck (++ discard p) p

alterIf :: (a -> a) -> (a -> Bool) -> a -> a
alterIf = liftA3 bool id

alterWhere :: (a -> a) -> (a -> Bool) -> [a] -> [a]
alterWhere f p = fmap (alterIf f p)

alterElem :: Eq b => (a -> b) -> (a -> a) -> b -> [a] -> [a]
alterElem on f x = alterWhere f ((==) x . on)

alterPlayer :: Player p => (p -> p) -> CandidateId -> [p] -> [p]
alterPlayer = alterElem playerId

alterPlayerInState :: (CompletePlayer -> CompletePlayer) -> CandidateId -> PlayState -> PlayState
alterPlayerInState alteration pid g = g { players = alterPlayer alteration pid $ players g}

playerExists :: Player p => CandidateId -> [p] -> Bool
playerExists pid = any ((==) pid . playerId)

cardBelongsToPlayer :: Player p => (p -> [Card]) -> Card -> CandidateId -> [p] -> Bool
cardBelongsToPlayer area card pid ps = (elem card . area <$> find ((==) pid . playerId) ps) == Just True
