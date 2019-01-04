module UpdateTests where

import Update
import Candidate
import Command
import GameState
import Player
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Card

import Data.List
import Control.Applicative

import ArbitraryInstances
import CardOrder
import Test.Hspec
import Test.QuickCheck

updateTests :: SpecWith ()
updateTests = describe "update" $ do

  describe "add player to new game" $
    it "adds player" $ property $ \pids pid ->
      length (players (update (AddPlayer pid) (New pids))) === length pids + 1

  describe "mark players ready" $
    it "begins preparing supply" $ property $
      isPreparingSupply . update MarkPlayersReady . New

  describe "place card in supply" $
    it "adds card" $ property $ \ps cards card ->
      length (supply (update (PlaceCardInSupply card) (PreparingSupply ps cards))) === length cards + 1

  describe "mark supply prepared" $
    it "begins preparing decks" $ property $ \ps cards ->
      isPreparingDecks $ update MarkSupplyPrepared $ PreparingSupply ps cards

  describe "add card to deck of player" $
    it "adds card to deck of player" $ property $ \(SelectedPlayerPreparingStartingDeck ps pid) cards card ->
      verifyPlayerPreparingStartingDeckUpdate
        pid
        (length . PlayerPreparingStartingDeck.deck)
        (+1)
        (AddCardToDeck pid card)
        (PreparingDecks ps cards)

  describe "mark decks prepared" $
    it "begins drawing initial hands" $ property $ \ps cards ->
      isDrawingInitialHands $ update MarkDecksPrepared $ PreparingDecks ps cards

  describe "draw card" $ do
    it "adds card to hand" $ property $ \(CardInStartingDeck ps pid card) cards ->
      verifyPlayerDrawingInitialHandUpdate
        pid
        (length . PlayerDrawingInitialHand.hand)
        (+1)
        (DrawCard pid card)
        (DrawingInitialHands ps cards)

    it "does not alter dominion of player" $ property $ \(CardInStartingDeck ps pid card) cards ->
      verifyPlayerDrawingInitialHandUpdate
        pid
        dominionWhileDrawingInitialHand
        id
        (DrawCard pid card)
        (DrawingInitialHands ps cards)

  describe "mark initial hands drawn" $
    it "transitions to in progress" $ property $ \ps cards ->
      isInProgress $ update MarkInitialHandsDrawn $ DrawingInitialHands ps cards

  describe "gain card" $
    it "adds card to discard" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) ->
      verifyPlayerUpdate pid (length . Player.discard) (+1) (GainCard pid card) (InProgress ps cards)

verifyPlayerUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (Player -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyPlayerUpdate pid = verifyElementUpdate (find ((==) pid . Player.playerId)) players

verifyPlayerDrawingInitialHandUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (PlayerDrawingInitialHand -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyPlayerDrawingInitialHandUpdate pid = verifyElementUpdate
  (find ((==) pid . PlayerDrawingInitialHand.playerId))
  (\x -> case x of
    DrawingInitialHands drawers _ -> drawers
    _ -> [])

verifyPlayerPreparingStartingDeckUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (PlayerPreparingStartingDeck -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyPlayerPreparingStartingDeckUpdate pid = verifyElementUpdate
  (find ((==) pid . PlayerPreparingStartingDeck.playerId))
  (\x -> case x of
    PreparingDecks preppers _ -> preppers
    _ -> [])

verifyElementUpdate :: (Eq b, Show b) =>
  ([a] -> Maybe a)
  -> (GameState -> [a])
  -> (a -> b)
  -> (b -> b)
  -> Command
  -> GameState
  -> Property
verifyElementUpdate element collection prop change = verifyUpdate (fmap prop . element . collection) (fmap change)

verifyUpdate :: (Eq a, Show a) =>
  (GameState -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyUpdate prop change command = liftA2 (===) (prop . update command) (change . prop)

isPreparingSupply :: GameState -> Bool
isPreparingSupply (PreparingSupply _ _) = True
isPreparingSupply _ = False

isPreparingDecks :: GameState -> Bool
isPreparingDecks (PreparingDecks _ _) = True
isPreparingDecks _ = False

isDrawingInitialHands :: GameState -> Bool
isDrawingInitialHands (DrawingInitialHands _ _) = True
isDrawingInitialHands _ = False

isInProgress :: GameState -> Bool
isInProgress (InProgress _ _) = True
isInProgress _ = False

dominionWhileDrawingInitialHand :: PlayerDrawingInitialHand -> [Card]
dominionWhileDrawingInitialHand p =
  sortOn arbitraryCardOrder $ concatMap ($ p) [PlayerDrawingInitialHand.deck, PlayerDrawingInitialHand.hand]

dominion :: Player -> [Card]
dominion p = sortOn arbitraryCardOrder $ concatMap ($ p) [Player.deck, Player.hand, Player.discard]

cardsInPlay :: GameState -> [Card]
cardsInPlay gameState = concatMap dominion (players gameState) ++ supply gameState
