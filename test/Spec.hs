{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Parsing
import EvaluationParameters
import Card
import Strategy
import Player

import Data.ByteString (ByteString)
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main = hspec $

  describe "GameDefinition.parse" $

    it "parses simple definition" $
      Parsing.parseEvaluationParameters ([r|
            { players:
              [ { id: first, strategy: [Province, Gold, Duchy, Silver, Estate] }
              , { id: second, strategy: [Gold, Silver, Copper] }
              ]
            }
          |] :: ByteString)
        `shouldBe` Right ( EvaluationParameters
          [ Player (PlayerId "first") (Strategy [Province, Gold, Duchy, Silver, Estate])
          , Player (PlayerId "second") (Strategy [Gold, Silver, Copper])
          ])
