{-# LANGUAGE InstanceSigs #-}

module Ruby.Reader.Parser.Result
  ( Result(..)

  , catchE
  , failE
  , returnE
  , thenE

  , parseError
  ) where

import Ruby.Reader.Lexer.Tokens

data Result a
  = Ok a
  | Failed String
  deriving (Show, Eq)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f r = case r of
    Ok a     -> Ok (f a)
    Failed b -> Failed b

thenE :: Result a -> (a -> Result b) -> Result b
m `thenE` k = case m of
  Ok a     -> k a
  Failed e -> Failed e

returnE :: a -> Result a
returnE = Ok

failE :: String -> Result a
failE = Failed

catchE :: Result a -> (String -> Result a) -> Result a
catchE m k = case m of
  Ok a     -> Ok a
  Failed e -> k e

parseError :: [Token] -> Result a
parseError tokens = failE $ "Parse error: " ++ errorMessage
  where
    errorMessage :: String
    errorMessage =
        case tokens of
          []    -> "unexpected EOF"
          (t:_) -> "at " ++ show t
