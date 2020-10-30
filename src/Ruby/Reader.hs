{-# LANGUAGE LambdaCase #-}

module Ruby.Reader
  ( readRuby
  ) where

import Ruby.Dsl
import Ruby.Reader.Parser.Result
import Ruby.Reader.Parser.Parser
import Ruby.Reader.Lexer.Lexer

readRuby :: Ruby expr => String -> expr ()
readRuby = flip (.) (happyParser . alexScanTokens) $ \case
  Ok f     -> f
  Failed e -> error $ show e
