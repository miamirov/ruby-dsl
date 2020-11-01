{-# LANGUAGE AllowAmbiguousTypes #-}

module Ruby.Dsl
  ( Ruby(..)

  , Command
  , Link
  , Name
  ) where

-- | Type alias for names of Ruby values
type Name = String

-- | Command mark
data Command

-- | Link mark
data Link

-- | DSL description for Ruby
class Ruby expr where
  -- | File with code
  file_
    :: [expr Command]
    -> expr ()

  -- | Defining function in current scope
  def_
    :: Name
    -> [Name]
    -> [expr Command]
    -> expr Command

  -- | Associate name with some value
  assoc_
    :: Name
    -> expr Link
    -> expr Command

  -- | If command
  if_
    :: expr Link
    -> [expr Command]
    -> Maybe [expr Command]
    -> expr Command

  -- | For command
  for_
    :: Name
    -> (expr Link, expr Link)
    -> [expr Command]
    -> expr Command

  -- | Return command
  return_
    :: expr Link
    -> expr Command

  -- | Call function and return it value
  call_
    :: Name
    -> [expr Link]
    -> expr Link

  -- | Call value as command
  onTop_
    :: expr Link
    -> expr Command

  -- | Read variable by name
  read_
    :: Name
    -> expr Link

  -- | Unary operator (-)
  negate_ :: expr Link -> expr Link

  -- | Unary operator (!)
  not_ :: expr Link -> expr Link

  -- | Binary operator (*)
  mult_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (/)
  div_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (+)
  plus_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (-)
  minus_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (>)
  gt_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (>=)
  gte_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (<)
  lt_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (<=)
  lte_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (==)
  eq_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (!=)
  notEq_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (&&)
  and_ :: expr Link -> expr Link -> expr Link

  -- | Binary operator (||)
  or_ :: expr Link -> expr Link -> expr Link

  -- | Create Integer literal
  int_ :: Integer -> expr Link

  -- | Create Float literal
  float_ :: Double -> expr Link

  -- | Create String literal
  string_ :: String -> expr Link

  -- | Create Bool literal
  bool_ :: Bool -> expr Link

  -- | Create Nil literal
  nil_ :: () -> expr Link
