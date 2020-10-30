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
    STR                               { Token T.StringL   _ $$ }
    TRUE                              { Token T.TrueL     _ _  }
    FALSE                             { Token T.FalseL    _ _  }
    '+'                               { Token T.PlusS     _ _  }
    '-'                               { Token T.MinusS    _ _  }
    '*'                               { Token T.AsteriskS _ _  }
    '/'                               { Token T.SlashS    _ _  }
    '=='                              { Token T.EqS       _ _  }
    '!='                              { Token T.NotEqS    _ _  }
    '>='                              { Token T.GteS      _ _  }
    '<='                              { Token T.LteS      _ _  }
    '>'                               { Token T.GtS       _ _  }
    '<'                               { Token T.LtS       _ _  }
    '&&'                              { Token T.AndS      _ _  }
    '||'                              { Token T.OrS       _ _  }
    '!'                               { Token T.NotS      _ _  }
    '('                               { Token T.RParS     _ _  }
    ')'                               { Token T.LParS     _ _  }
    '..'                              { Token T.DDotS     _ _  }
    DEF                               { Token T.DefK      _ _  }
    FOR                               { Token T.ForK      _ _  }
    END                               { Token T.EndK      _ _  }
    IF                                { Token T.IfK       _ _  }
    IN                                { Token T.InK       _ _  }
    ELSE                              { Token T.ElseK     _ _  }
    ELSIF                             { Token T.ElsifK    _ _  }
    WORD                              { Token T.Word      _ $$ }
    FLOAT                             { Token T.FloatL    _ $$ }
    INT                               { Token T.IntL      _ $$ }

%%

File
  :                               { [] }