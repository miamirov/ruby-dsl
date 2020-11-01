module Main
  ( main
  ) where

import Test.Tasty (defaultMain)

import Test.Suites

main :: IO ()
main = defaultMain testRubyDsl
