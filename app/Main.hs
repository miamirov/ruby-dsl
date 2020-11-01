{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (openFile, IOMode(ReadMode), hGetContents)
import System.Environment (getArgs)

import Ruby.Interpreter
import Ruby.Formatter
import Ruby.Reader

import Ruby.Reader.Lexer.Lexer

main :: IO ()
main = do
  getArgs >>= \case
    ["tokens", source] -> readSource source >>= print . alexScanTokens
    ["interpret", source] -> readSource source >>= interpret_ . readRuby
    ["format", source] -> readSource source >>= putStrLn . format_ . readRuby
    _ -> error $
      "Invalid arguments:\n" ++
      "Use:\n" ++
      "  *run* interpret <source file>\n" ++
      "or\n" ++
      "  *run* format <source file>\n"
  where
    readSource :: FilePath -> IO String
    readSource source =
      openFile source ReadMode >>= hGetContents
