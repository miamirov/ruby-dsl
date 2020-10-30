{-# LANGUAGE AllowAmbiguousTypes #-}

module Ruby.Dsl
  ( Ruby(..)

  , Command
  , Link
  , Name
  ) where

type Name = String

data Command

data Link

class Ruby expr where
  file_
    :: [expr Command]
    -> expr ()

  def_
    :: Name
    -> [Name]
    -> [expr Command]
    -> expr Command

  assoc_
    :: Name
    -> expr Link
    -> expr Command

  if_
    :: expr Link
    -> [expr Command]
    -> Maybe [expr Command]
    -> expr Command

  for_
    :: Name
    -> (expr Link, expr Link)
    -> [expr Command]
    -> expr Command

  return_
    :: expr Link
    -> expr Command

  call_
    :: Name
    -> [expr Link]
    -> expr Link

  onTop_
    :: expr Link
    -> expr Command

  read_
    :: Name
    -> expr Link

  negate_ :: expr Link -> expr Link

  not_ :: expr Link -> expr Link

  mult_ :: expr Link -> expr Link -> expr Link

  div_ :: expr Link -> expr Link -> expr Link

  plus_ :: expr Link -> expr Link -> expr Link

  minus_ :: expr Link -> expr Link -> expr Link

  gt_ :: expr Link -> expr Link -> expr Link

  gte_ :: expr Link -> expr Link -> expr Link

  lt_ :: expr Link -> expr Link -> expr Link

  lte_ :: expr Link -> expr Link -> expr Link

  eq_ :: expr Link -> expr Link -> expr Link

  notEq_ :: expr Link -> expr Link -> expr Link

  and_ :: expr Link -> expr Link -> expr Link

  or_ :: expr Link -> expr Link -> expr Link

  int_ :: Integer -> expr Link

  float_ :: Double -> expr Link

  string_ :: String -> expr Link

  bool_ :: Bool -> expr Link

  nil_ :: () -> expr Link
