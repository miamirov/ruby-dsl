module Main where

import Ruby.Formatter
import Ruby.Reader

import Ruby.Reader.Interpreter

main :: IO ()
main = interpret_ $ readRuby code

code :: String
code =
  "a = gets.chomp.to_i(12);\n" ++
  "if a <= 0\n" ++
  "then\n" ++
  "  puts(-1);\n" ++
  "else\n" ++
  "  puts(1);\n" ++
  "end\n"