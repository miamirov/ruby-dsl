{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Ruby.Formatter
  ( Formatter(..)

  , format_
  ) where

import Ruby.Dsl
import Data.List (intercalate)

-- | Format Ruby code
newtype Formatter a = Formatter { format :: Int -> String }

-- | Call formatting without offset
format_ :: Formatter a -> String
format_ = flip format 0

-- | Create offset by its level
buildOffset :: Int -> String
buildOffset offset = replicate (4 * offset) ' '

-- | Ruby implementation
instance Ruby Formatter where
  file_
    :: [Formatter Command]
    -> Formatter ()
  file_ = Formatter . flip formatCommandList

  def_
    :: Name
    -> [Name]
    -> [Formatter Command]
    -> Formatter Command
  def_ fName args commandList = Formatter $ \offset -> do
    let ff = buildOffset offset
    ff ++ "def " ++ fName ++ "(" ++ intercalate ", " args ++ ")\n" ++ 
      formatCommandList (succ offset) commandList ++
      ff ++ "end"

  assoc_
    :: Name
    -> Formatter Link
    -> Formatter Command
  assoc_ vName link = Formatter $ \offset ->
    buildOffset offset ++ vName ++ " = " ++ format_ link ++ ";"

  if_
    :: Formatter Link
    -> [Formatter Command]
    -> Maybe [Formatter Command]
    -> Formatter Command
  if_ condition trueCommandList mbFalseCommandList = Formatter $ \offset -> do
    let ff = buildOffset offset
    ff ++ "if " ++ format_ condition ++ "\n" ++
      ff ++ "then\n" ++
      formatCommandList (succ offset) trueCommandList ++
      case mbFalseCommandList of
        Nothing -> ""
        Just falseCommandList ->
          ff ++ "else\n" ++
            formatCommandList (succ offset) falseCommandList
      ++ ff ++ "end"

  for_
    :: Name
    -> (Formatter Link, Formatter Link)
    -> [Formatter Command]
    -> Formatter Command
  for_ iName (from, to) commandList = Formatter $ \offset -> do
    let ff = buildOffset offset
    ff ++ "for " ++ iName ++ " in " ++ format_ from ++ ".." ++ format_ to ++ " do\n" ++
      formatCommandList (succ offset) commandList ++
      ff ++ "end"

  return_
    :: Formatter Link
    -> Formatter Command
  return_ link = Formatter $ \offset ->
    buildOffset offset ++
      case format_ link of
        "nil" -> "return;"
        fl    -> "return " ++ fl ++ ";"

  call_
    :: Name
    -> [Formatter Link]
    -> Formatter Link
  call_ fName links = Formatter $ \offset ->
    buildOffset offset ++ fName ++ "(" ++ intercalate ", " (format_ <$> links) ++ ")" 

  onTop_
    :: Formatter Link
    -> Formatter Command
  onTop_ link = Formatter $ \offset ->
    buildOffset offset ++ format_ link ++ ";"

  read_
    :: Name
    -> Formatter Link
  read_ = Formatter . const

  negate_ :: Formatter Link -> Formatter Link
  negate_ = formatUnaryOperator "-"

  not_ :: Formatter Link -> Formatter Link
  not_ = formatUnaryOperator "!"

  mult_ :: Formatter Link -> Formatter Link -> Formatter Link
  mult_ = formatBinaryOperator "*"

  div_ :: Formatter Link -> Formatter Link -> Formatter Link
  div_ = formatBinaryOperator "/"

  plus_ :: Formatter Link -> Formatter Link -> Formatter Link
  plus_ = formatBinaryOperator "+"

  minus_ :: Formatter Link -> Formatter Link -> Formatter Link
  minus_ = formatBinaryOperator "-"

  gt_ :: Formatter Link -> Formatter Link -> Formatter Link
  gt_ = formatBinaryOperator ">"

  gte_ :: Formatter Link -> Formatter Link -> Formatter Link
  gte_ = formatBinaryOperator ">="

  lt_ :: Formatter Link -> Formatter Link -> Formatter Link
  lt_ = formatBinaryOperator "<"

  lte_ :: Formatter Link -> Formatter Link -> Formatter Link
  lte_ = formatBinaryOperator "<="

  eq_ :: Formatter Link -> Formatter Link -> Formatter Link
  eq_ = formatBinaryOperator "=="

  notEq_ :: Formatter Link -> Formatter Link -> Formatter Link
  notEq_ = formatBinaryOperator "!="

  and_ :: Formatter Link -> Formatter Link -> Formatter Link
  and_ = formatBinaryOperator "&&"

  or_ :: Formatter Link -> Formatter Link -> Formatter Link
  or_ = formatBinaryOperator "||"

  int_ :: Integer -> Formatter Link
  int_ = formatLiteral

  float_ :: Double -> Formatter Link
  float_ = formatLiteral

  string_ :: String -> Formatter Link
  string_ = formatLiteral

  bool_ :: Bool -> Formatter Link
  bool_ = formatLiteral_ $ \case { True -> "true"; False -> "false" }

  nil_ :: () -> Formatter Link
  nil_ = formatLiteral_ (const "nil")

formatCommandList :: Int -> [Formatter Command] -> String
formatCommandList offset = concat . ((++ "\n") . flip format offset <$>)

formatLiteral :: Show a => a -> Formatter Link
formatLiteral = formatLiteral_ show

formatLiteral_ :: (a -> String) -> a -> Formatter Link
formatLiteral_ show_ = Formatter . const . show_

formatUnaryOperator :: String -> Formatter Link -> Formatter Link
formatUnaryOperator operator value =  Formatter . const $
  operator ++ format_ value

formatBinaryOperator :: String -> Formatter Link -> Formatter Link -> Formatter Link 
formatBinaryOperator operator left right = Formatter . const $
  "(" ++ format_ left ++ " " ++ operator ++ " " ++ format_ right ++ ")" 