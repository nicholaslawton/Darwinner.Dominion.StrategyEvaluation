module Game
  ( Game(state, gen)
  , record
  , mapState
  , history
  , Game.new
  , Game.players
  ) where

import GameState
import Message
import Event
import CompletePlayer

import System.Random

data Game = Game
  { state :: GameState
  , events :: [Event]
  , gen :: StdGen
  }

mapState :: (GameState -> GameState) -> Game -> Game
mapState f game = game { Game.state = f (Game.state game) }

record :: Message -> Game -> Game
record msg game = game { events = event msg : events game }

event :: Message -> Event
event (AddPlayer pid) = PlayerAdded pid
event MarkPlayersReady = PlayersReady
event (PlaceCardInSupply card) = CardPlacedInSupply card
event MarkSupplyPrepared = SupplyPrepared
event (AddCardToDeck pid card) = CardAddedToDeck pid card
event MarkDecksPrepared = DecksPrepared
event MarkInitialHandsDrawn = InitialHandsDrawn
event (DrawCard pid card) = CardDrawn pid card
event (GainCard pid card) = CardGained pid card
event (PlayTreasureCard pid card) = TreasureCardPlayed pid card
event (DiscardUnplayedCard pid card) = UnplayedCardDiscarded pid card
event (DiscardPlayedCard pid card) = PlayedCardDiscarded pid card
event (ReformDeck pid) = DeckReformed pid
event BuyPhaseComplete = BuyPhaseCompleted
event DiscardStepComplete = HandAndPlayedCardsDiscarded
event DrawHandStepComplete = NextHandDrawn
event EndTurn = TurnEnded
event EndGame = GameEnded

history :: Game -> [Event]
history = reverse . events

new :: Int -> Game
new = Game (New []) [] . mkStdGen

players :: Game -> [CompletePlayer]
players = GameState.players . state
