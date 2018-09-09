{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy
import Network.HTTP.Types.Status
import Web.Scotty

import Parsing

main :: IO ()
main = scotty 3000 $
  post "/evaluate" $
    Parsing.parseEvaluationParameters . toStrict <$> body >>= toStatus
  where
    toStatus (Left _) = status badRequest400
    toStatus (Right _) = status ok200
