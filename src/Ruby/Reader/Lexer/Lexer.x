{
module Ruby.Reader.Lexer.Lexer
  ( alexScanTokens
  ) where

import Ruby.Reader.Lexer.Tokens
}

%wrapper "posn"

$digit_ = 1-9
$digit  = 0-9
$alpha  = [a-zA-Z_]

tokens :-
    $white+                  ;

    $alpha [$digit $alpha]*  { token Word }

    0 | ($digit_ $digit*)    { token Digit }

{
convertPosition :: AlexPosn -> Position
convertPosition (AlexPn g l c) = Position g l c

token :: TokenType -> AlexPosn -> String -> Token
token t p s = Token t (convertPosition p) s
}