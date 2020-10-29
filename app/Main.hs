module Main where

import Ruby.Reader.Lexer.Lexer
import Ruby.Reader.Parser.Parser

main :: IO ()
main = do
  print $ happyParser $ alexScanTokens ""
  print $ happyParser $ alexScanTokens "111"
  print $ happyParser $ alexScanTokens "_v"
