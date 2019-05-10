{-# OPTIONS_GHC -fno-warn-orphans #-} 
module ArbitraryInstances where

import Coins
import BuyAllowance
import Card
import EvaluationParameters
import Player
import CompletePlayer
import PlayerWithoutDominion
import PlayerWithDeck
import PlayerWithHand
import Candidate
import CandidateId
import Strategy
import PlayState
import Turn

import Control.Applicative
import Control.Monad
import Data.Bifunctor

import Test.QuickCheck

instance Arbitrary Coins where
  arbitrary = Coins <$> arbitrary

instance Arbitrary BuyAllowance where
  arbitrary = BuyAllowance <$> arbitrary

instance Arbitrary Card where
  arbitrary = oneof [return Province, return Duchy, return Estate, return Gold, return Silver, return Copper]

instance Arbitrary Candidate where
  arbitrary = liftA2 Candidate arbitrary arbitrary

instance Arbitrary CompletePlayer where
  arbitrary = liftM5 CompletePlayer.new arbitrary arbitrary arbitrary arbitrary arbitrary

instance Arbitrary PlayerWithoutDominion where
  arbitrary = PlayerWithoutDominion.new <$> arbitrary

instance Arbitrary PlayerWithDeck where
  arbitrary = liftA2 PlayerWithDeck.new arbitrary arbitrary

instance Arbitrary PlayerWithHand where
  arbitrary = liftA3 PlayerWithHand.new arbitrary arbitrary arbitrary

instance Arbitrary CandidateId where
  arbitrary = CandidateId <$> arbitrary

instance Arbitrary Strategy where
  arbitrary = Strategy <$> arbitrary

instance Arbitrary EvaluationParameters where
  arbitrary = EvaluationParameters <$> validCandidates

instance Arbitrary Turn where
  arbitrary = Turn . getPositive <$> arbitrary

instance Arbitrary PlayState where
  arbitrary = liftA3 PlayState arbitrary arbitrary arbitrary

data ValidCandidateIds
  = TwoCandidateIds CandidateId CandidateId
  | ThreeCandidateIds CandidateId CandidateId CandidateId
  | FourCandidateIds CandidateId CandidateId CandidateId CandidateId
  deriving (Eq, Show)

instance Arbitrary ValidCandidateIds where
  arbitrary = oneof
    [ liftA2 TwoCandidateIds
        (uniqueId '1' <$> arbitrary)
        (uniqueId '2' <$> arbitrary)
    , liftA3 ThreeCandidateIds
        (uniqueId '1' <$> arbitrary)
        (uniqueId '2' <$> arbitrary)
        (uniqueId '3' <$> arbitrary)
    , liftM4 FourCandidateIds
        (uniqueId '1' <$> arbitrary)
        (uniqueId '2' <$> arbitrary)
        (uniqueId '3' <$> arbitrary)
        (uniqueId '4' <$> arbitrary)
    ]
    where
      uniqueId :: Char -> CandidateId -> CandidateId
      uniqueId x (CandidateId cid) = CandidateId $ x : cid

validCandidateIds :: ValidCandidateIds -> [CandidateId]
validCandidateIds (TwoCandidateIds id1 id2) = [id1, id2]
validCandidateIds (ThreeCandidateIds id1 id2 id3) = [id1, id2, id3]
validCandidateIds (FourCandidateIds id1 id2 id3 id4) = [id1, id2, id3, id4]

validCandidate :: CandidateId -> Gen Candidate
validCandidate cid = Candidate cid <$> arbitrary

validCandidates :: Gen [Candidate]
validCandidates = validCandidateIds <$> arbitrary >>= traverse validCandidate

validPlayersPreparingStartingDecks :: Gen [PlayerWithDeck]
validPlayersPreparingStartingDecks = validCandidateIds <$> arbitrary
  >>= traverse (\cid -> PlayerWithDeck.new cid <$> arbitrary)

validPlayersDrawingInitialHands :: Gen [PlayerWithHand]
validPlayersDrawingInitialHands = validCandidateIds <$> arbitrary
  >>= traverse (\cid -> liftA2 (PlayerWithHand.new cid) arbitrary arbitrary)

validPlayer :: CandidateId -> Gen CompletePlayer
validPlayer cid = liftM4 (CompletePlayer.new cid) arbitrary arbitrary arbitrary arbitrary

validPlayers :: Gen [CompletePlayer]
validPlayers = validCandidateIds <$> arbitrary >>= traverse validPlayer

data ValidPlayers = ValidPlayers [CompletePlayer] 
  deriving (Eq, Show)

instance Arbitrary ValidPlayers where
  arbitrary = ValidPlayers <$> validPlayers

data ValidPlayersWithParams = ValidPlayersWithParams [CompletePlayer] EvaluationParameters
  deriving (Eq, Show)

instance Arbitrary ValidPlayersWithParams where
  arbitrary = fmap combine $ validCandidateIds <$> arbitrary >>= traverse validPlayerAndCandidate
    where
      validPlayerAndCandidate :: CandidateId -> Gen (CompletePlayer, Candidate)
      validPlayerAndCandidate cid = validPlayer cid >>= liftA2 fmap (,) (validCandidate . playerId)
      combine :: [(CompletePlayer, Candidate)] -> ValidPlayersWithParams
      combine = uncurry ValidPlayersWithParams . second EvaluationParameters . unzip

data SelectedPlayerWithDeck = SelectedPlayerWithDeck [PlayerWithDeck] CandidateId
  deriving (Eq, Show)

instance Arbitrary SelectedPlayerWithDeck where
  arbitrary = validPlayersPreparingStartingDecks
    >>= fmap (uncurry SelectedPlayerWithDeck . second playerId)
      . selectedElement

data SelectedPlayer = SelectedPlayer PlayState CandidateId
  deriving (Eq, Show)

instance Arbitrary SelectedPlayer where
  arbitrary = do
    cards <- arbitrary
    t <- arbitrary
    validPlayers >>= fmap (\(ps, p) -> SelectedPlayer (PlayState ps cards t) (playerId p)) . selectedElement

data SelectedPlayerAndCardInSupply = SelectedPlayerAndCardInSupply PlayState CandidateId Card
  deriving (Eq, Show)

instance Arbitrary SelectedPlayerAndCardInSupply where
  arbitrary = do
    (ps, p) <- validPlayers >>= selectedElement
    (cards, card) <- arbitrary `suchThat` (not . null) >>= selectedElement
    t <- arbitrary
    return $ SelectedPlayerAndCardInSupply (PlayState ps cards t) (playerId p) card

data CardInStartingDeck = CardInStartingDeck [PlayerWithHand] [Card] CandidateId Card
  deriving (Eq, Show)

instance Arbitrary CardInStartingDeck where
  arbitrary =
    selectedCardInArea CardInStartingDeck deck validPlayersDrawingInitialHands

data CardInDeck = CardInDeck PlayState CandidateId Card
  deriving (Eq, Show)

instance Arbitrary CardInDeck where
  arbitrary = selectedCardInAreaWithPlayState CardInDeck deck

data CardInHand = CardInHand PlayState CandidateId Card
  deriving (Eq, Show)

instance Arbitrary CardInHand where
  arbitrary = selectedCardInAreaWithPlayState CardInHand hand

data PlayedCard = PlayedCard PlayState CandidateId Card
  deriving (Eq, Show)

instance Arbitrary PlayedCard where
  arbitrary = selectedCardInAreaWithPlayState PlayedCard playedCards

selectedElement :: [a] -> Gen ([a], a)
selectedElement = selectFrom elements

selectedElementMatching :: (a -> Bool) -> [a] -> Gen ([a], a)
selectedElementMatching predicate = selectedElement . filter predicate

selectFrom :: (a -> Gen b) -> a -> Gen (a, b)
selectFrom f x = (,) x <$> f x 

selectedCardInArea :: Player p => ([p] -> [Card] -> CandidateId -> Card -> b) -> (p -> [Card]) -> Gen [p] -> Gen b
selectedCardInArea c area validPs = do
  cards <- arbitrary
  validPs `suchThat` (not . null . concatMap area)
    >>= selectedElementMatching (not . null . area)
    >>= \(ps, p) -> c ps cards (playerId p) <$> elements (area p)

selectedCardInAreaWithPlayState :: (PlayState -> CandidateId -> Card -> a) -> (CompletePlayer -> [Card]) -> Gen a
selectedCardInAreaWithPlayState c area = do
  t <- arbitrary
  selectedCardInArea (\ps cards -> c $ PlayState ps cards t) area validPlayers
  