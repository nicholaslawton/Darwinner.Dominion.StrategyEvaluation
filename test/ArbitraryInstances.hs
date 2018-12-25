{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import Control.Applicative
import Control.Monad
import Test.QuickCheck

import Card
import EvaluationParameters
import Player
import Candidate
import Strategy

instance Arbitrary Card where
  arbitrary = oneof [return Province, return Duchy, return Estate, return Gold, return Silver, return Copper]

instance Arbitrary Candidate where
  arbitrary = liftA2 Candidate arbitrary arbitrary

instance Arbitrary Player where
  arbitrary = liftA3 Player arbitrary arbitrary arbitrary

instance Arbitrary CandidateId where
  arbitrary = CandidateId <$> arbitrary

instance Arbitrary Strategy where
  arbitrary = Strategy <$> arbitrary

instance Arbitrary EvaluationParameters where
  arbitrary = EvaluationParameters . validCandidates <$> arbitrary

data ValidCandidates
  = TwoCandidates Candidate Candidate
  | ThreeCandidates Candidate Candidate Candidate
  | FourCandidates Candidate Candidate Candidate Candidate
  deriving (Eq, Show)

instance Arbitrary ValidCandidates where
  arbitrary = oneof
    [ liftA2 TwoCandidates
        (uniqueCandidate '1' <$> arbitrary)
        (uniqueCandidate '2' <$> arbitrary)
    , liftA3 ThreeCandidates
        (uniqueCandidate '1' <$> arbitrary)
        (uniqueCandidate '2' <$> arbitrary)
        (uniqueCandidate '3' <$> arbitrary)
    , liftM4 FourCandidates
        (uniqueCandidate '1' <$> arbitrary)
        (uniqueCandidate '2' <$> arbitrary)
        (uniqueCandidate '3' <$> arbitrary)
        (uniqueCandidate '4' <$> arbitrary)
    ]
    where
      uniqueCandidate :: Char -> Candidate -> Candidate
      uniqueCandidate x c = c { candidateId = mapCandidateId ((:) x) $ candidateId c }

      mapCandidateId :: (String -> String) -> CandidateId -> CandidateId
      mapCandidateId f (CandidateId cid) = CandidateId $ f cid

validCandidates :: ValidCandidates -> [Candidate]
validCandidates (TwoCandidates c1 c2) = [c1, c2]
validCandidates (ThreeCandidates c1 c2 c3) = [c1, c2, c3]
validCandidates (FourCandidates c1 c2 c3 c4) = [c1, c2, c3, c4]

data PlayersAndSelectedPlayer = PlayersAndSelectedPlayer [Player] CandidateId
  deriving (Eq, Show)

instance Arbitrary PlayersAndSelectedPlayer where
  arbitrary = suchThat arbitrary (not . null)
    >>= \ps -> PlayersAndSelectedPlayer ps <$> selectPlayer ps
    where
      selectPlayer :: [Player] -> Gen CandidateId
      selectPlayer = elements . fmap playerId

data CardInDeck = CardInDeck [Player] CandidateId Card
  deriving (Eq, Show)

instance Arbitrary CardInDeck where
  arbitrary = suchThat arbitrary (not . null . concatMap deck)
    >>= \ps -> uncurry (CardInDeck ps) <$> selectPlayerAndCard ps
    where
      selectPlayerAndCard :: [Player] -> Gen (CandidateId, Card)
      selectPlayerAndCard ps = (elements . filter (not . null . deck)) ps
        >>= \p -> (,) (playerId p) <$> (elements . deck) p
