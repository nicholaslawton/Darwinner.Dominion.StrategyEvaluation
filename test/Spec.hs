module Main where

import ParsingTests

import Test.Hspec

main :: IO ()
main = hspec $
  parsingTests
