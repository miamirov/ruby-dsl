module Main where

import Ruby.Reader.Lexer.Lexer
import Ruby.Reader.Parser.Parser
import Ruby.Reader.Parser.Result
import Ruby.Dsl

main :: IO ()
main = do
  let f :: Ruby expr => Result (expr ())
      f = happyParser $ alexScanTokens ""
  return ()
