{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy
import Network.HTTP.Types.Status
import Web.Scotty

import GameDefinition

main :: IO ()
main = scotty 3000 $
  post "/evaluate" $
    GameDefinition.parse . toStrict <$> body >>= toStatus
  where
    toStatus (Left _) = status badRequest400
    toStatus (Right _) = status ok200
