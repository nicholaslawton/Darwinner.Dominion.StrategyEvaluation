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

  describe "parseEvaluationParameters" $ do

    it "parses simple definition" $
      parseEvaluationParameters ([r|
            { players:
              [ { id: first, strategy: [Province, Gold, Duchy, Silver, Estate] }
              , { id: second, strategy: [Gold, Silver, Copper] }
              ]
            }
          |] :: ByteString)
        `shouldBe` Right (EvaluationParameters
          [ Player (PlayerId "first") (Strategy [Province, Gold, Duchy, Silver, Estate])
          , Player (PlayerId "second") (Strategy [Gold, Silver, Copper])
          ])
    
    it "rejects a single player" $
      parseEvaluationParameters ([r|
            { players:
              [ { id: one, strategy: [Province, Gold, Duchy, Silver, Estate] }
              ]
            }
          |] :: ByteString)
        `shouldBe` Left (ParseError "Insufficient number of players (minimum two)")
    
    it "rejects five players" $
      parseEvaluationParameters ([r|
            { players:
              [ { id: one, strategy: [Province, Gold, Duchy, Silver, Estate] }
              , { id: two, strategy: [Province, Gold, Duchy, Silver, Estate] }
              , { id: three, strategy: [Province, Gold, Duchy, Silver, Estate] }
              , { id: four, strategy: [Province, Gold, Duchy, Silver, Estate] }
              , { id: five, strategy: [Province, Gold, Duchy, Silver, Estate] }
              ]
            }
          |] :: ByteString)
        `shouldBe` Left (ParseError "Too many players (maximum four)")
