{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParsingTests where

import Parsing
import EvaluationParameters
import Card
import Strategy
import Candidate
import CandidateId

import Data.ByteString (ByteString)
import Test.Hspec
import Text.RawString.QQ

parsingTests :: SpecWith ()
parsingTests = describe "parseEvaluationParameters" $ do

  it "parses simple definition" $
    parseEvaluationParameters ([r|
        first: Province, Gold, Duchy, Silver, Estate
        second: Gold, Silver, Copper |] :: ByteString)
      `shouldBe` Right (EvaluationParameters
        [ Candidate (CandidateId "first") (Strategy [Province, Gold, Duchy, Silver, Estate])
        , Candidate (CandidateId "second") (Strategy [Gold, Silver, Copper])
        ])
  
  it "rejects a single player" $
    parseEvaluationParameters ([r| one: Province, Gold, Duchy, Silver, Estate |] :: ByteString)
      `shouldBe` Left (ParseError "Insufficient number of players (minimum two)")
  
  it "rejects five players" $
    parseEvaluationParameters ([r|
        one: Province, Gold, Duchy, Silver, Estate
        two: Province, Gold, Duchy, Silver, Estate
        three: Province, Gold, Duchy, Silver, Estate
        four: Province, Gold, Duchy, Silver, Estate
        five: Province, Gold, Duchy, Silver, Estate |] :: ByteString)
      `shouldBe` Left (ParseError "Too many players (maximum four)")

  it "rejects duplicate identifiers" $
    parseEvaluationParameters ([r|
        same: Province, Gold, Duchy, Silver, Estate
        same: Gold, Silver, Copper |] :: ByteString)
      `shouldBe` Left (ParseError "Duplicate candidate identifiers")
