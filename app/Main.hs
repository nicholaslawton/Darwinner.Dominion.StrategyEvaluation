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
import Event
import Game
import Engine

main :: IO ()
main = scotty 3000 $
  post "/evaluate" $
    fmap (evaluate 0) . parseEvaluationParameters . toStrict <$> body
      >>= response

evaluate :: Int -> EvaluationParameters -> [Event]
evaluate seed parameters = history (execState (runReaderT Engine.run parameters) (Game.new seed))

response :: (Show a, Show b) => Either a b -> ActionM ()
response (Left a) = stringContent a >> status badRequest400
response (Right b) = stringContent b >> status ok200

stringContent :: Show a => a -> ActionM ()
stringContent = text . pack . show

toStatus :: Either a b -> ActionM ()
toStatus (Left _) = status badRequest400
toStatus (Right _) = status ok200
