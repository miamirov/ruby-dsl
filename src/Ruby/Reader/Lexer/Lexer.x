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
    $white+                           ;

    \;                                { token SemicolonS }
    \"[^\"]*\"                        { token StringL    }

    true                              { token TrueL   }
    false                             { token FalseL  }
    return                            { token ReturnK }
    nil                               { token NilL    }

    \+                                { token PlusS     }
    \-                                { token MinusS    }
    \*                                { token AsteriskS }
    \/                                { token SlashS    }

    \=\=                              { token EqS    }
    \!\=                              { token NotEqS }

    \>\=                              { token GteS }
    \<\=                              { token LteS }
    \>                                { token GtS  }
    \<                                { token LtS  }
    \&\&                              { token AndS }
    \|\|                              { token OrS  }
    \!                                { token NotS }

    \(                                { token RParS }
    \)                                { token LParS }
    \.\.                              { token DDotS }

    def                               { token DefK   }
    for                               { token ForK   }
    end                               { token EndK   }
    if                                { token IfK    }
    in                                { token InK    }
    else                              { token ElseK  }
    elsif                             { token ElsifK }

    $alpha [$digit $alpha]*           { token Word }

    (0 | ($digit_ $digit*))\.$digit+  { token FloatL }
    0 | ($digit_ $digit*)             { token IntL   }

{
convertPosition :: AlexPosn -> Position
convertPosition (AlexPn g l c) = Position g l c

token :: TokenType -> AlexPosn -> String -> Token
token t p s = Token t (convertPosition p) s
}