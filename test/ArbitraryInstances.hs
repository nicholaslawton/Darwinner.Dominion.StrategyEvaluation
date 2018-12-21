{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import Control.Applicative
import Control.Monad
import Test.QuickCheck

import Card
import EvaluationParameters
import Player
import Strategy

instance Arbitrary Card where
  arbitrary = oneof [return Province, return Duchy, return Estate, return Gold, return Silver, return Copper]

instance Arbitrary Player where
  arbitrary = liftA3 Player arbitrary arbitrary arbitrary

instance Arbitrary PlayerId where
  arbitrary = PlayerId <$> arbitrary

instance Arbitrary Strategy where
  arbitrary = Strategy <$> arbitrary

instance Arbitrary EvaluationParameters where
  arbitrary = EvaluationParameters . validPlayers <$> arbitrary

data ValidPlayers
  = TwoPlayers Player Player
  | ThreePlayers Player Player Player
  | FourPlayers Player Player Player Player
  deriving (Eq, Show)

instance Arbitrary ValidPlayers where
  arbitrary = oneof
    [ liftA2 TwoPlayers
        (uniquePlayer '1' <$> arbitrary)
        (uniquePlayer '2' <$> arbitrary)
    , liftA3 ThreePlayers
        (uniquePlayer '1' <$> arbitrary)
        (uniquePlayer '2' <$> arbitrary)
        (uniquePlayer '3' <$> arbitrary)
    , liftM4 FourPlayers
        (uniquePlayer '1' <$> arbitrary)
        (uniquePlayer '2' <$> arbitrary)
        (uniquePlayer '3' <$> arbitrary)
        (uniquePlayer '4' <$> arbitrary)
    ]
    where
      uniquePlayer :: Char -> Player -> Player
      uniquePlayer x p = p { playerId = mapPlayerId ((:) x) $ playerId p }

      mapPlayerId :: (String -> String) -> PlayerId -> PlayerId
      mapPlayerId f (PlayerId pid) = PlayerId $ f pid

validPlayers :: ValidPlayers -> [Player]
validPlayers (TwoPlayers p1 p2) = [p1, p2]
validPlayers (ThreePlayers p1 p2 p3) = [p1, p2, p3]
validPlayers (FourPlayers p1 p2 p3 p4) = [p1, p2, p3, p4]

data PlayersAndSelectedPlayer = PlayersAndSelectedPlayer [Player] PlayerId
  deriving (Eq, Show)

instance Arbitrary PlayersAndSelectedPlayer where
  arbitrary = suchThat arbitrary (not . null)
    >>= \s -> PlayersAndSelectedPlayer s <$> selectPlayer s
    where
      selectPlayer :: [Player] -> Gen PlayerId
      selectPlayer = elements . fmap playerId
