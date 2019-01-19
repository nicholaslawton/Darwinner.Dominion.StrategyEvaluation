module UpdateTests where

import Update
import Candidate
import Command
import GameState
import GenericPlayer
import Player
import PlayerWithoutDominion
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Card
import BuyAllowance

import Data.List
import Control.Applicative

import GameStateValidation
import ArbitraryInstances
import CardOrder
import Test.Hspec
import Test.QuickCheck

updateTests :: SpecWith ()
updateTests = describe "update" $ do

  describe "add player to new game" $
    it "adds player" $ property $ \cids ->
      let (pid : pids) = validCandidateIds cids
      in length (players (update (AddPlayer pid) (New (PlayerWithoutDominion <$> pids)))) === length pids + 1

  describe "mark players ready" $
    it "begins preparing supply" $ property $
      preparingSupply . update MarkPlayersReady . New

  describe "place card in supply" $
    it "adds card" $ property $ \ps cards card ->
      length (supply (update (PlaceCardInSupply card) (PreparingSupply ps cards))) === length cards + 1

  describe "mark supply prepared" $
    it "begins preparing decks" $ property $ \ps cards ->
      preparingDecks $ update MarkSupplyPrepared $ PreparingSupply ps cards

  describe "add card to deck of player" $
    it "adds card to deck of player" $ property $ \(SelectedPlayerPreparingStartingDeck ps pid) cards card ->
      verifyPlayerPreparingStartingDeckUpdate
        pid
        (length . GenericPlayer.deck)
        (+1)
        (AddCardToDeck pid card)
        (PreparingDecks ps cards)

  describe "mark decks prepared" $
    it "begins drawing initial hands" $ property $ \ps cards ->
      drawingInitialHands $ update MarkDecksPrepared $ PreparingDecks ps cards

  describe "draw card" $ do
    it "adds card to hand" $ property $ \(CardInStartingDeck ps pid card) cards ->
      verifyPlayerDrawingInitialHandUpdate
        pid
        (length . GenericPlayer.hand)
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
    it "transitions to buy phase" $ property $ \ps cards ->
      inBuyPhase $ update MarkInitialHandsDrawn $ DrawingInitialHands ps cards

  describe "gain card" $ do
    it "adds card to discard" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) (Positive buys) ->
      verifyPlayerUpdate pid (length . GenericPlayer.discard) (+1) (GainCard pid card) (BuyPhase (BuyAllowance buys) ps cards)

    it "does not alter cards in play" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) (Positive buys) ->
      verifyUpdate cardsInPlay id (GainCard pid card) (BuyPhase (BuyAllowance buys) ps cards)

    it "decrements buy allowance" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) (Positive buys) ->
      verifyUpdate buyAllowance (subtract 1) (GainCard pid card) (BuyPhase (BuyAllowance buys) ps cards)

  describe "buy phase completion" $
    it "transitions to clean up phase" $ property $ \ps cards ->
      inCleanUpPhase $ update BuyPhaseComplete $ BuyPhase (BuyAllowance 0) ps cards

  describe "discard card" $ do
    it "removes card from hand" $ property $ \(CardInHand ps pid card) cards ->
      verifyPlayerUpdate pid (length . GenericPlayer.hand) (subtract 1) (DiscardCard pid card) (CleanUpPhase ps cards)

    it "adds card to discard" $ property $ \(CardInHand ps pid card) cards ->
      verifyPlayerUpdate pid (length . GenericPlayer.discard) (+1) (DiscardCard pid card) (CleanUpPhase ps cards)

    it "does not alter dominion of player" $ property $ \(CardInHand ps pid card) cards ->
      verifyPlayerUpdate pid dominion id (DiscardCard pid card) (CleanUpPhase ps cards)

verifyPlayerUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (Player -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyPlayerUpdate pid = verifyElementUpdate (find ((==) pid . GenericPlayer.playerId)) players

verifyPlayerDrawingInitialHandUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (PlayerDrawingInitialHand -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyPlayerDrawingInitialHandUpdate pid = verifyElementUpdate
  (find ((==) pid . GenericPlayer.playerId))
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
  (find ((==) pid . GenericPlayer.playerId))
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

dominionWhileDrawingInitialHand :: PlayerDrawingInitialHand -> [Card]
dominionWhileDrawingInitialHand p =
  sortOn arbitraryCardOrder $ concatMap ($ p) [GenericPlayer.deck, GenericPlayer.hand]

dominion :: Player -> [Card]
dominion p = sortOn arbitraryCardOrder $ concatMap ($ p) [GenericPlayer.deck, GenericPlayer.hand, GenericPlayer.discard]

cardsInPlay :: GameState -> [Card]
cardsInPlay gameState = sortOn arbitraryCardOrder $ concatMap ($ gameState) [concatMap dominion . players, supply]

buyAllowance :: GameState -> Int
buyAllowance (BuyPhase (BuyAllowance buys) _ _) = buys
buyAllowance _ = 0
