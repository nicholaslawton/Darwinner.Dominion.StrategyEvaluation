module Collections
  ( alterIf
  , alterWhere
  , alterElem
  ) where

import Control.Applicative
import Data.Bool

alterIf :: (a -> a) -> (a -> Bool) -> a -> a
alterIf = liftA3 bool id

alterWhere :: (a -> a) -> (a -> Bool) -> [a] -> [a]
alterWhere f p = fmap (alterIf f p)

alterElem :: Eq b => (a -> b) -> (a -> a) -> b -> [a] -> [a]
alterElem on f x = alterWhere f ((==) x . on)
