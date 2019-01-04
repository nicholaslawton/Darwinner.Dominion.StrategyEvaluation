{-# OPTIONS_GHC -fno-warn-orphans #-} 
module ArbitraryInstances where

import Card
import EvaluationParameters
import Player
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Candidate
import Strategy

import Data.Bifunctor
import Control.Applicative
import Control.Monad

import Test.QuickCheck

instance Arbitrary Card where
  arbitrary = oneof [return Province, return Duchy, return Estate, return Gold, return Silver, return Copper]

instance Arbitrary Candidate where
  arbitrary = liftA2 Candidate arbitrary arbitrary

instance Arbitrary Player where
  arbitrary = liftM4 Player arbitrary arbitrary arbitrary arbitrary

instance Arbitrary PlayerPreparingStartingDeck where
  arbitrary = liftA2 PlayerPreparingStartingDeck arbitrary arbitrary

instance Arbitrary PlayerDrawingInitialHand where
  arbitrary = liftA3 PlayerDrawingInitialHand arbitrary arbitrary arbitrary

instance Arbitrary CandidateId where
  arbitrary = CandidateId <$> arbitrary

instance Arbitrary Strategy where
  arbitrary = Strategy <$> arbitrary

instance Arbitrary EvaluationParameters where
  arbitrary = EvaluationParameters <$> validCandidates

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

validCandidates :: Gen [Candidate]
validCandidates = validCandidateIds <$> arbitrary >>= traverse (\cid -> Candidate cid <$> arbitrary)

validPlayersPreparingStartingDecks :: Gen [PlayerPreparingStartingDeck]
validPlayersPreparingStartingDecks = validCandidateIds <$> arbitrary
  >>= traverse (\cid -> PlayerPreparingStartingDeck cid <$> arbitrary)

validPlayersDrawingInitialHands :: Gen [PlayerDrawingInitialHand]
validPlayersDrawingInitialHands = validCandidateIds <$> arbitrary
  >>= traverse (\cid -> liftA2 (PlayerDrawingInitialHand cid) arbitrary arbitrary)

validPlayers :: Gen [Player]
validPlayers = validCandidateIds <$> arbitrary
  >>= traverse (\cid -> liftA3 (Player cid) arbitrary arbitrary arbitrary)

data SelectedPlayerPreparingStartingDeck =
  SelectedPlayerPreparingStartingDeck [PlayerPreparingStartingDeck] CandidateId
  deriving (Eq, Show)

instance Arbitrary SelectedPlayerPreparingStartingDeck where
  arbitrary = validPlayersPreparingStartingDecks
    >>= fmap (uncurry SelectedPlayerPreparingStartingDeck . second PlayerPreparingStartingDeck.playerId)
      . selectedElement

data SelectedPlayer = SelectedPlayer [Player] CandidateId
  deriving (Eq, Show)

instance Arbitrary SelectedPlayer where
  arbitrary = validPlayers >>= fmap (uncurry SelectedPlayer . second Player.playerId) . selectedElement

data CardInStartingDeck = CardInStartingDeck [PlayerDrawingInitialHand] CandidateId Card
  deriving (Eq, Show)

instance Arbitrary CardInStartingDeck where
  arbitrary = validPlayersDrawingInitialHands `suchThat` (not . null . concatMap PlayerDrawingInitialHand.deck)
    >>= selectedElementMatching (not . null . PlayerDrawingInitialHand.deck)
    >>= \(ps, p) -> CardInStartingDeck ps (PlayerDrawingInitialHand.playerId p) <$> elements (PlayerDrawingInitialHand.deck p)

data CardInSupply = CardInSupply [Card] Card
  deriving (Eq, Show)

instance Arbitrary CardInSupply where
  arbitrary = arbitrary >>= fmap (uncurry CardInSupply) . selectedElement

selectedElement :: [a] -> Gen ([a], a)
selectedElement = selectFrom elements

selectedElementMatching :: (a -> Bool) -> [a] -> Gen ([a], a)
selectedElementMatching predicate = selectedElement . filter predicate

selectFrom :: (a -> Gen b) -> a -> Gen (a, b)
selectFrom f x = (,) x <$> f x 
