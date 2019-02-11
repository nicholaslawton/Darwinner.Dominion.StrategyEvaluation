module UpdateTests where

import Update
import Candidate
import Command
import GameState
import PlayState hiding (players, supply)
import Player
import CompletePlayer
import PlayerWithoutDominion
import Card
import BuyAllowance

import Data.List
import Control.Applicative

import GameStateValidation
import PlayerValidation
import ArbitraryInstances
import CardOrder
import Test.Hspec
import Test.QuickCheck hiding (Discard, discard)

updateTests :: SpecWith ()
updateTests = describe "update" $ do

  describe "add player to new game" $
    it "adds player" $ property $ \cids ->
      let (pid : pids) = validCandidateIds cids
      in length (players (update (AddPlayer pid) (New (PlayerWithoutDominion.new <$> pids)))) === length pids + 1

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
    it "adds card to deck of player" $ property $ \(SelectedPlayerWithDeck ps pid) cards card ->
      verifyPlayerUpdate pid (length . deck) (+1) (AddCardToDeck pid card) (PreparingDecks ps cards)

  describe "mark decks prepared" $
    it "begins drawing initial hands" $ property $ \ps cards ->
      drawingInitialHands $ update MarkDecksPrepared $ PreparingDecks ps cards

  describe "mark initial hands drawn" $
    it "transitions to buy phase" $ property $ \ps cards ->
      buyPhase $ update MarkInitialHandsDrawn $ DrawingInitialHands ps cards

  describe "draw card" $ do
    describe "for initial hand" $
      drawCardProperties (\(CardInStartingDeck ps pid card) -> (ps, pid, card)) DrawingInitialHands

    describe "during clean up phase" $
      drawCardProperties
        (\(CardInDeck ps pid card) -> (ps, pid, card))
        (\ps cards -> CleanUpPhase (PlayState ps cards) DrawHand)

  describe "gain card" $ do
    it "adds card to discard" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) (Positive buys) ->
      verifyPlayerUpdate pid (length . discard) (+1) (GainCard pid card) (BuyPhase (PlayState ps cards) (BuyAllowance buys))

    it "does not alter cards in play" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) (Positive buys) ->
      verifyUpdate cardsInPlay id (GainCard pid card) (BuyPhase (PlayState ps cards) (BuyAllowance buys))

    it "decrements buy allowance" $ property $ \(SelectedPlayer ps pid) (CardInSupply cards card) (Positive buys) ->
      verifyUpdate buyAllowance (subtract 1) (GainCard pid card) (BuyPhase (PlayState ps cards) (BuyAllowance buys))

  describe "discard card" $ do
    it "removes card from hand" $ property $ \(CardInHand ps pid card) cards ->
      verifyPlayerUpdate pid (length . hand) (subtract 1) (DiscardCard pid card) (CleanUpPhase (PlayState ps cards) Discard)

    it "adds card to discard" $ property $ \(CardInHand ps pid card) cards ->
      verifyPlayerUpdate pid (length . discard) (+1) (DiscardCard pid card) (CleanUpPhase (PlayState ps cards) Discard)

    it "does not alter dominion of player" $ property $ \(CardInHand ps pid card) cards ->
      verifyPlayerUpdate pid dominion id (DiscardCard pid card) (CleanUpPhase (PlayState ps cards) Discard)

  describe "reform deck" $ do
    it "leaves discard empty" $ property $ \(SelectedPlayer ps pid) cards ->
      verifyPlayerState pid (null . discard) $ update (ReformDeck pid) (CleanUpPhase (PlayState ps cards) DrawHand)

    it "does not alter combined deck and discard" $ property $ \(SelectedPlayer ps pid) cards ->
      verifyPlayerUpdate pid (liftA2 (++) deck discard) id (ReformDeck pid) (CleanUpPhase (PlayState ps cards) DrawHand)

  describe "buy phase completion" $
    it "transitions to clean up phase" $ property $ \ps cards ->
      cleanUpPhase $ update BuyPhaseComplete $ BuyPhase (PlayState ps cards) (BuyAllowance 0)

  describe "discard step completion" $
    it "transitions to draw next hand step" $ property $ \ps cards ->
      drawHandStep $ update DiscardStepComplete $ CleanUpPhase (PlayState ps cards) Discard

drawCardProperties :: (Arbitrary a, Show a, Player p) =>
  (a -> ([p], CandidateId, Card))
  -> ([p] -> [Card] -> GameState)
  -> SpecWith ()
drawCardProperties unpack constructGame = do
  it "adds card to hand" $ property $ \cardInDeck cards ->
    let (ps, pid, card) = unpack cardInDeck
    in verifyPlayerUpdate pid (length . hand) (+1) (DrawCard pid card) (constructGame ps cards)

  it "does not alter dominion of player" $ property $ \cardInDeck cards ->
    let (ps, pid, card) = unpack cardInDeck
    in verifyPlayerUpdate pid dominion id (DrawCard pid card) (constructGame ps cards)

verifyPlayerUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (CompletePlayer -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyPlayerUpdate pid prop change =
  verifyUpdate (fmap prop . find ((==) pid . playerId) . players) (fmap change)

verifyUpdate :: (Eq a, Show a) =>
  (GameState -> a)
  -> (a -> a)
  -> Command
  -> GameState
  -> Property
verifyUpdate prop change command = liftA2 (===) (prop . update command) (change . prop)

verifyPlayerState :: CandidateId -> (CompletePlayer -> Bool) -> GameState -> Property
verifyPlayerState pid verification game =
  fmap verification (find ((==) pid . playerId) (players game)) === Just True

cardsInPlay :: GameState -> [Card]
cardsInPlay gameState = sortOn arbitraryCardOrder $ concatMap ($ gameState) [concatMap dominion . players, supply]

buyAllowance :: GameState -> Int
buyAllowance (BuyPhase _ (BuyAllowance buys)) = buys
buyAllowance _ = 0
