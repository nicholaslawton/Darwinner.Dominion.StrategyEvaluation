{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParsingTests where

import Parsing
import EvaluationParameters
import Card
import Strategy
import Player

import Data.ByteString (ByteString)
import Test.Hspec
import Text.RawString.QQ

parsingTests :: SpecWith ()
parsingTests = describe "parseEvaluationParameters" $ do

  it "parses simple definition" $
    parseEvaluationParameters ([r|
          { players:
            [ { id: first, strategy: [Province, Gold, Duchy, Silver, Estate] }
            , { id: second, strategy: [Gold, Silver, Copper] }
            ]
          }
        |] :: ByteString)
      `shouldBe` Right (EvaluationParameters
        [ Player.new (PlayerId "first") (Strategy [Province, Gold, Duchy, Silver, Estate])
        , Player.new (PlayerId "second") (Strategy [Gold, Silver, Copper])
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

  it "rejects duplicate identifiers" $
    parseEvaluationParameters ([r|
          { players:
            [ { id: same, strategy: [Province, Gold, Duchy, Silver, Estate] }
            , { id: same, strategy: [Gold, Silver, Copper] }
            ]
          }
        |] :: ByteString)
      `shouldBe` Left (ParseError "Duplicate player identifiers")
