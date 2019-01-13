module BuyAllowance
  ( BuyAllowance(..)
  , initial )
  where

newtype BuyAllowance = BuyAllowance Int
  deriving (Eq, Show)

initial :: BuyAllowance
initial = BuyAllowance 1
