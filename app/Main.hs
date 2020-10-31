module Main where

import Ruby.Formatter
import Ruby.Reader

main :: IO ()
main = getLine >>= putStrLn . format_ . readRuby