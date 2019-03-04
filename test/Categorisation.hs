module Categorisation (categorise) where

import Control.Applicative
import Data.Map

categorise :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
categorise key value xs = fromListWith (++) $ liftA2 (,) key ((: []) . value) <$> xs
