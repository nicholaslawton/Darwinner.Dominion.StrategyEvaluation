{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (pack)
import Network.HTTP.Types.Status
import Web.Scotty

import Parsing
import EvaluationParameters
import Command
import Game
import Engine

main :: IO ()
main = scotty 3000 $
  post "/evaluate" $
    fmap (evaluate 0) . parseEvaluationParameters . toStrict <$> body
      >>= response

evaluate :: Int -> EvaluationParameters -> [Event]
evaluate seed parameters = history (execState (runReaderT Engine.run parameters) (Game.new seed))

response :: (Show a) => Either a [Event] -> ActionM ()
response (Left a) = showContent a >> status badRequest400
response (Right events) = stringContent (formatEvents events) >> status ok200

formatEvents :: [Event] -> String
formatEvents = foldr (\a b -> show a ++ "\n" ++ b) ""

showContent :: Show a => a -> ActionM ()
showContent = stringContent . show

stringContent :: String -> ActionM ()
stringContent = text . pack
