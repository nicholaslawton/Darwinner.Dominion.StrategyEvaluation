module UpdateTests where

import Update
import CandidateId
import Message
import GameState
import PlayState hiding (players)
import Player
import CompletePlayer
import PlayerWithoutDominion
import Card
import Coins
import BuyAllowance
import Turn

import Data.List
import Data.Composition
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
      length (GameState.supply (update (PlaceCardInSupply card) (PreparingSupply ps cards))) === length cards + 1

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
      drawCardProperties
        (\(CardInStartingDeck ps cards pid card) -> (ps, cards, pid, card))
        (\ps cards _ -> DrawingInitialHands ps cards)

    describe "during clean up phase" $
      drawCardProperties
        (\(CardInDeck (PlayState ps cards _) pid card) -> (ps, cards, pid, card))
        (CleanUpPhase DrawHand .:. PlayState)

  describe "gain card" $ do
    it "adds card to discard" $ property $ \(SelectedPlayerAndCardInSupply g pid card) coins (Positive buys) ->
      verifyPlayerUpdate pid (length . discard) (+1) (GainCard pid card) (BuyPhase coins (BuyAllowance buys) g)

    it "does not alter cards in play" $ property $ \(SelectedPlayerAndCardInSupply g pid card) coins (Positive buys) ->
      verifyUpdate cardsInPlay id (GainCard pid card) (BuyPhase coins (BuyAllowance buys) g)

    it "decrements buy allowance" $ property $ \(SelectedPlayerAndCardInSupply g pid card) coins (Positive buys) ->
      verifyUpdate buyAllowance (subtract 1) (GainCard pid card) (BuyPhase coins (BuyAllowance buys) g)

  describe "playing treasure card" $ do
    it "removes card from hand" $ property $ \(CardInHand g pid card) coins buys ->
      verifyPlayerUpdate pid (length . hand) (subtract 1) (PlayTreasureCard pid card) (BuyPhase coins buys g)

    it "increases the coins to spend" $ property $ \(CardInHand g pid card) coins buys ->
      verifyUpdate coinBalance (+ value card) (PlayTreasureCard pid card) (BuyPhase coins buys g)

    it "adds card to played cards" $ property $ \(CardInHand g pid card) coins buys ->
      verifyPlayerUpdate pid (length . playedCards) (+1) (PlayTreasureCard pid card) (BuyPhase coins buys g)
    
  describe "discard unplayed card" $ do
    it "removes card from hand" $ property $ \(CardInHand g pid card) ->
      verifyPlayerUpdate pid (length . hand) (subtract 1) (DiscardCard pid card) (CleanUpPhase Discard g)

    it "adds card to discard" $ property $ \(CardInHand g pid card) ->
      verifyPlayerUpdate pid (length . discard) (+1) (DiscardCard pid card) (CleanUpPhase Discard g)

    it "does not alter dominion of player" $ property $ \(CardInHand g pid card) ->
      verifyPlayerUpdate pid dominion id (DiscardCard pid card) (CleanUpPhase Discard g)

  describe "discard played card" $ do
    it "removes card from played cards" $ property $ \(PlayedCard g pid card) ->
      verifyPlayerUpdate pid (length . playedCards) (subtract 1) (DiscardPlayedCard pid card) (CleanUpPhase Discard g)

    it "adds card to discard" $ property $ \(PlayedCard g pid card) ->
      verifyPlayerUpdate pid (length . discard) (+1) (DiscardPlayedCard pid card) (CleanUpPhase Discard g)

  describe "reform deck" $ do
    it "leaves discard empty" $ property $ \(SelectedPlayer g pid) ->
      verifyPlayerState pid (null . discard) $ update (ReformDeck pid) (CleanUpPhase DrawHand g)

    it "does not alter combined deck and discard" $ property $ \(SelectedPlayer g pid) ->
      verifyPlayerUpdate pid (liftA2 (++) deck discard) id (ReformDeck pid) (CleanUpPhase DrawHand g)

  describe "buy phase completion" $
    it "transitions to clean up phase" $ property $ \coins ->
      cleanUpPhase . update BuyPhaseComplete . BuyPhase coins (BuyAllowance 0)

  describe "discard step completion" $
    it "transitions to draw next hand step" $ property $
      drawHandStep . update DiscardStepComplete . CleanUpPhase Discard

  describe "draw next hand step completion" $
    it "transitions to turn end" $ property $
      turnEnd . update DrawHandStepComplete . CleanUpPhase DrawHand

  describe "end turn" $
    it "transitions to next turn" $ property $
      buyPhase . update EndTurn . TurnEnd

  describe "end game" $
    it "transitions to game over" $ property $
      gameOver . update EndGame . TurnEnd

drawCardProperties :: (Arbitrary a, Show a, Player p)
  => (a -> ([p], [Card], CandidateId, Card))
  -> ([p] -> [Card] -> Turn -> GameState)
  -> SpecWith ()
drawCardProperties unpack constructGame = do
  it "adds card to hand" $ property $ \cardInDeck ->
    let (ps, cards, pid, card) = unpack cardInDeck
    in verifyPlayerUpdate pid (length . hand) (+1) (DrawCard pid card) . constructGame ps cards

  it "does not alter dominion of player" $ property $ \cardInDeck ->
    let (ps, cards, pid, card) = unpack cardInDeck
    in verifyPlayerUpdate pid dominion id (DrawCard pid card) . constructGame ps cards

verifyPlayerUpdate :: (Eq a, Show a) =>
  CandidateId
  -> (CompletePlayer -> a)
  -> (a -> a)
  -> Message
  -> GameState
  -> Property
verifyPlayerUpdate pid prop change =
  verifyUpdate (fmap prop . find ((==) pid . playerId) . players) (fmap change)

verifyUpdate :: (Eq a, Show a) =>
  (GameState -> a)
  -> (a -> a)
  -> Message
  -> GameState
  -> Property
verifyUpdate prop change message = liftA2 (===) (prop . update message) (change . prop)

verifyPlayerState :: CandidateId -> (CompletePlayer -> Bool) -> GameState -> Property
verifyPlayerState pid verification game =
  fmap verification (find ((==) pid . playerId) (players game)) === Just True

cardsInPlay :: GameState -> [Card]
cardsInPlay gameState = sortOn arbitraryCardOrder $ concatMap ($ gameState) [concatMap dominion . players, GameState.supply]

buyAllowance :: GameState -> Int
buyAllowance (BuyPhase _ (BuyAllowance buys) _) = buys
buyAllowance _ = 0

coinBalance :: GameState -> Coins
coinBalance (BuyPhase coins _ _) = coins
coinBalance _ = Coins 0
