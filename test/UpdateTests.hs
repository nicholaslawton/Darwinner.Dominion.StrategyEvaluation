module UpdateTests where

import Update
import Candidate
import Command
import GameState
import Player
import PlayerPreparingStartingDeck
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
      let
        playersPreparingStartingDecks (PreparingDecks preppers _) = preppers
        playersPreparingStartingDecks _ = []
      in
        fmap (length . PlayerPreparingStartingDeck.deck) (find ((==) pid . PlayerPreparingStartingDeck.playerId) (playersPreparingStartingDecks (update (AddCardToDeck pid card) (PreparingDecks ps cards))))
          === fmap ((+1) . length . PlayerPreparingStartingDeck.deck) (find ((==) pid . PlayerPreparingStartingDeck.playerId) ps)
      --verifyPlayerUpdate (length . deck) (+1) ps pid (AddCardToDeck pid card) (PreparingDecks ps cards)

  describe "mark decks prepared" $
    it "begins drawing initial hands" $ property $ \ps cards ->
      isDrawingInitialHands $ update MarkDecksPrepared $ PreparingDecks ps cards

  describe "draw card" $ do
    it "adds card to hand" $ property $ \(CardInDeck ps pid card) cards ->
      verifyPlayerUpdate (length . hand) (+1) ps pid (DrawCard pid card) (DrawingInitialHands ps cards)

    it "does not alter dominion of player" $ property $ \(CardInDeck ps pid card) cards ->
      verifyPlayerUpdate dominion id ps pid (DrawCard pid card) (DrawingInitialHands ps cards)

  describe "mark initial hands drawn" $
    it "transitions to prepared" $ property $ \ps cards ->
      isPrepared $ update MarkInitialHandsDrawn $ DrawingInitialHands ps cards

verifyPlayerUpdate :: (Eq a, Show a) =>
  (Player -> a)
  -> (a -> a)
  -> [Player]
  -> CandidateId
  -> Command
  -> GameState
  -> Property
verifyPlayerUpdate prop change ps pid command gameState =
  fmap prop (findPlayer pid (players (update command gameState))) === fmap (change . prop) (findPlayer pid ps)

findPlayer :: CandidateId -> [Player] -> Maybe Player
findPlayer pid = find ((==) pid . Player.playerId)

isPreparingSupply :: GameState -> Bool
isPreparingSupply (PreparingSupply _ _) = True
isPreparingSupply _ = False

isPreparingDecks :: GameState -> Bool
isPreparingDecks (PreparingDecks _ _) = True
isPreparingDecks _ = False

isDrawingInitialHands :: GameState -> Bool
isDrawingInitialHands (DrawingInitialHands _ _) = True
isDrawingInitialHands _ = False

isPrepared :: GameState -> Bool
isPrepared Prepared = True
isPrepared _ = False

dominion :: Player -> [Card]
dominion = sortOn arbitraryCardOrder . liftA2 (++) Player.deck hand
