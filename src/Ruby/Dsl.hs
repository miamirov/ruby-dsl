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
  file
    :: [expr Command]
    -> expr ()

  def_
    :: [Name]
    -> [expr Command]
    -> expr Command

  assoc
    :: Name
    -> expr Link
    -> expr Command

  if_
    :: [(expr Link, [expr Command])]
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

  call
    :: Name
    -> [expr Link]
    -> expr Link

  onTop
    :: expr Link
    -> expr Command

  read_
    :: Name
    -> expr Link

  negate :: expr Link -> expr Link

  not    :: expr Link -> expr Link

  mult   :: expr Link -> expr Link -> expr Link

  div    :: expr Link -> expr Link -> expr Link

  plus   :: expr Link -> expr Link -> expr Link

  minus  :: expr Link -> expr Link -> expr Link

  gt     :: expr Link -> expr Link -> expr Link

  gte    :: expr Link -> expr Link -> expr Link

  lt     :: expr Link -> expr Link -> expr Link

  lte    :: expr Link -> expr Link -> expr Link

  eq     :: expr Link -> expr Link -> expr Link

  notEq  :: expr Link -> expr Link -> expr Link

  and    :: expr Link -> expr Link -> expr Link

  or     :: expr Link -> expr Link -> expr Link

  int    :: Integer -> expr Link
  
  float  :: Double -> expr Link
  
  string :: String -> expr Link
  
  bool   :: Bool -> expr Link
