{
module Ruby.Reader.Parser.Parser
  ( happyParser
  ) where

import Ruby.Reader.Lexer.Tokens (Token(..))
import qualified Ruby.Reader.Lexer.Tokens as T
import Ruby.Reader.Parser.Result
}

%name           happyParser
%tokentype      { Token }
%error          { parseError }
%monad          { Result } { thenE } { returnE }

%token
    WORD      { Token T.Word _ $$ }
    DIGIT     { Token T.Digit _ $$ }

%%

File
  :                               { []   }
  | WORD                          { [$1] }
  | DIGIT                         { [$1] }