{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Ruby.Reader.Lexer.Tokens
  ( Token(..)
  , TokenType(..)
  , Position(..)
  ) where

data Token = Token
  { tType     :: TokenType
  , tPosition :: Position
  , tValue    :: String
  } deriving (Eq)

instance Show Token where
  show :: Token -> String
  show Token { tType, tPosition, tValue } =
    "Token" ++
        "-" ++ show tType     ++
        " " ++ show tValue    ++
        " " ++ show tPosition

data Position = Position
  { pGlobal :: Int
  , pLine   :: Int
  , pColumn :: Int
  } deriving (Eq)

instance Show Position where
  show :: Position -> String
  show Position { pGlobal, pLine, pColumn } =
    "(g:" ++ show pGlobal ++
    " l:" ++ show pLine   ++
    " c:" ++ show pColumn ++
    ")"

data TokenType
  = Word
  | IntL
  | FloatL
  | StringL
  | TrueL
  | FalseL
  | NilL
  | DefK
  | EndK
  | IfK
  | ElseK
  | ThenK
  | ForK
  | InK
  | DoK
  | ReturnK
  | RParS
  | LParS
  | DDotS
  | PlusS
  | MinusS
  | AsteriskS
  | SlashS
  | EqS
  | NotEqS
  | GteS
  | GtS
  | LtS
  | LteS
  | AndS
  | OrS
  | NotS
  | SemicolonS
  | CommaS
  | AssignedS
  deriving (Eq, Show)
