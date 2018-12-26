module UpdateTests where

import Update
import Candidate
import Command
import GameState
import Player
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
    it "adds player" $ property $ \ps player ->
      length (players (update (AddPlayer player) (New ps))) === length ps + 1

  describe "players ready" $
    it "begins preparing supply" $ property $
      isPreparingSupply . update PlayersReady . New

  describe "place card in supply" $
    it "adds card" $ property $ \ps cards card ->
      length (supply (update (PlaceCardInSupply card) (PreparingSupply ps cards))) === length cards + 1

  describe "supply ready" $
    it "begins preparing decks" $ property $ \ps cards ->
      isPreparingDecks $ update SupplyReady $ PreparingSupply ps cards

  describe "add card to deck of player" $
    it "adds card to deck of player" $ property $ \(SelectedPlayer ps pid) cards card ->
      verifyPlayerUpdate (length . deck) (+1) ps pid (AddCardToDeck pid card) (PreparingDecks ps cards)

  describe "decks ready" $
    it "begins drawing initial hands" $ property $ \ps cards ->
      isDrawingInitialHands $ update DecksReady $ PreparingDecks ps cards

  describe "draw card" $ do
    it "adds card to hand" $ property $ \(CardInDeck ps pid card) cards ->
      verifyPlayerUpdate (length . hand) (+1) ps pid (DrawCard pid card) (DrawingInitialHands ps cards)

    it "does not alter dominion of player" $ property $ \(CardInDeck ps pid card) cards ->
      verifyPlayerUpdate dominion id ps pid (DrawCard pid card) (DrawingInitialHands ps cards)

verifyPlayerUpdate :: (Eq a, Show a) => (Player -> a) -> (a -> a) -> [Player] -> CandidateId -> Command -> GameState -> Property
verifyPlayerUpdate prop change ps pid command gameState =
  fmap prop (findPlayer pid (players (update command gameState)))
    === fmap (change . prop) (findPlayer pid ps)

findPlayer :: CandidateId -> [Player] -> Maybe Player
findPlayer pid = find ((==) pid . playerId)

isPreparingSupply :: GameState -> Bool
isPreparingSupply (PreparingSupply _ _) = True
isPreparingSupply _ = False

isPreparingDecks :: GameState -> Bool
isPreparingDecks (PreparingDecks _ _) = True
isPreparingDecks _ = False

isDrawingInitialHands :: GameState -> Bool
isDrawingInitialHands (DrawingInitialHands _ _) = True
isDrawingInitialHands _ = False

dominion :: Player -> [Card]
dominion = sortOn arbitraryCardOrder . liftA2 (++) deck hand
