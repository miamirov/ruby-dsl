{
{-# LANGUAGE RankNTypes #-}

module Ruby.Reader.Parser.Parser
  ( happyParser
  ) where

import Ruby.Reader.Lexer.Tokens (Token(..))
import qualified Ruby.Reader.Lexer.Tokens as T
import Ruby.Reader.Parser.Result
import Ruby.Dsl
}

%name           happyParser
%tokentype      { Token }
%error          { parseError }
%monad          { Result } { thenE } { returnE }

%token
    STR         { Token T.StringL    _ $$ }
    TRUE        { Token T.TrueL      _ _  }
    FALSE       { Token T.FalseL     _ _  }
    RETURN      { Token T.ReturnK    _ _  }
    NIL         { Token T.NilL       _ _  }
    '+'         { Token T.PlusS      _ _  }
    '-'         { Token T.MinusS     _ _  }
    '*'         { Token T.AsteriskS  _ _  }
    '/'         { Token T.SlashS     _ _  }
    '=='        { Token T.EqS        _ _  }
    '!='        { Token T.NotEqS     _ _  }
    '>='        { Token T.GteS       _ _  }
    '<='        { Token T.LteS       _ _  }
    '>'         { Token T.GtS        _ _  }
    '<'         { Token T.LtS        _ _  }
    '&&'        { Token T.AndS       _ _  }
    '||'        { Token T.OrS        _ _  }
    '!'         { Token T.NotS       _ _  }
    '('         { Token T.RParS      _ _  }
    ')'         { Token T.LParS      _ _  }
    '..'        { Token T.DDotS      _ _  }
    DO          { Token T.DoK        _ _  }
    DEF         { Token T.DefK       _ _  }
    FOR         { Token T.ForK       _ _  }
    END         { Token T.EndK       _ _  }
    IF          { Token T.IfK        _ _  }
    IN          { Token T.InK        _ _  }
    ELSE        { Token T.ElseK      _ _  }
    THEN        { Token T.ThenK      _ _  }
    WORD        { Token T.Word       _ $$ }
    FLOAT       { Token T.FloatL     _ $$ }
    STRING      { Token T.StringL    _ $$ }
    INT         { Token T.IntL       _ $$ }
    ';'         { Token T.SemicolonS _ _  }
    ','         { Token T.CommaS     _ _  }
    '='         { Token T.AssignedS  _ _  }


%%

File :: { forall expr . Ruby expr => expr () }
  : CommandList                 { file_ $1 }

CommandList :: { forall expr . Ruby expr => [expr Command] }
  : Command CommandList         { $1 : $2 }
  |                             { []      }

Command :: { forall expr . Ruby expr => expr Command }
  : Return ';'                  { $1 }
  | Assoc  ';'                  { $1 }
  | For                         { $1 }
  | If                          { $1 }
  | Def                         { $1 }
  | OnTop  ';'                  { $1 }

For :: { forall expr . Ruby expr => expr Command }
  : FOR WORD IN Link '..' Link DO CommandList END  { for_ $2 ($4, $6) $8 }

Return :: { forall expr . Ruby expr => expr Command }
  : RETURN                      { return_ $ nil_ () }
  | RETURN Link                 { return_ $2        }

Assoc :: { forall expr . Ruby expr => expr Command }
  : WORD '=' Link               { assoc_ $1 $3 }

If :: { forall expr . Ruby expr => expr Command }
  : IF Link THEN CommandList END                    { if_ $2 $4 Nothing   }
  | IF Link THEN CommandList ELSE CommandList END   { if_ $2 $4 (Just $6) }

Def :: { forall expr . Ruby expr => expr Command }
  : DEF WORD '(' ArgList ')' CommandList END { def_ $2 $4 $6 }

ArgList :: { [Name] }
  : WORD ',' ArgList            { $1 : $3 }
  | WORD                        { [$1] }
  |                             { [] }

OnTop :: { forall expr . Ruby expr => expr Command }
  : Link                        { onTop_ $1 }

Link :: { forall expr . Ruby expr => expr Link }
  : Or                          { $1 }

Or :: { forall expr . Ruby expr => expr Link }
  : Or '||' And                 { or_ $1 $3 }
  | And                         { $1        }

And :: { forall expr . Ruby expr => expr Link }
  : And '&&' Eq                 { and_ $1 $3 }
  | Eq                          { $1         }

Eq :: { forall expr . Ruby expr => expr Link }
  : Eq '==' Comp                { eq_    $1 $3 }
  | Eq '!=' Comp                { notEq_ $1 $3 }
  | Comp                        { $1           }

Comp :: { forall expr . Ruby expr => expr Link }
  : Comp '>'  Plus              { gt_  $1 $3 }
  | Comp '<'  Plus              { lt_  $1 $3 }
  | Comp '>=' Plus              { gte_ $1 $3 }
  | Comp '<=' Plus              { lte_ $1 $3 }
  | Plus                        { $1         }

Plus :: { forall expr . Ruby expr => expr Link }
  : Plus '+' Mult               { plus_  $1 $3 }
  | Plus '-' Mult               { minus_ $1 $3 }
  | Mult                        { $1           }

Mult :: { forall expr . Ruby expr => expr Link }
  : Mult '*' Unary              { mult_ $1 $3 }
  | Mult '/' Unary              { div_  $1 $3 }
  | Unary                       { $1          }

Unary :: { forall expr . Ruby expr => expr Link }
  : '!' Unary                   { not_    $2 }
  | '-' Unary                   { negate_ $2 }
  | Unit                        { $1         }

Unit :: { forall expr . Ruby expr => expr Link }
  : '(' Link ')'                { $2 }
  | Call                        { $1 }
  | Read                        { $1 }
  | Float                       { $1 }
  | Int                         { $1 }
  | String                      { $1 }
  | Bool                        { $1 }
  | Nil                         { $1 }

Call :: { forall expr . Ruby expr => expr Link }
  : WORD '(' LinkList ')'      { call_ $1 $3 }

LinkList :: { forall expr . Ruby expr => [expr Link] }
  : Link ',' LinkList           { $1 : $3 }
  | Link                        { [$1]    }
  |                             { []      }

Read :: { forall expr . Ruby expr => expr Link }
  : WORD                        { read_ $1 }

Float :: { forall expr . Ruby expr => expr Link }
  : FLOAT                       { float_ (read $1 :: Double) }

Int :: { forall expr . Ruby expr => expr Link }
  : INT                         { int_ (read $1 :: Integer) }

String :: { forall expr . Ruby expr => expr Link }
  : STRING                      { string_ (filter (== '"') $1) }

Bool :: { forall expr . Ruby expr => expr Link }
  : TRUE                        { bool_ True  }
  | FALSE                       { bool_ False }

Nil :: { forall expr . Ruby expr => expr Link }
  : NIL                         { nil_ () }
