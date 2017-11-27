-----------------------------------------------------------
--
--
--
-----------------------------------------------------------
module TokenDef (module StdTokenDef,tokenDef) where

import StdTokenDef
import Parser

tokenDef    = henk

henk  = emptyStyle
    { commentStart  = "{-"
    , commentEnd  = "-}"
    , commentLine  = "--"
    , nestedComments= True
    , identStart  = letter
    , identLetter  = alphaNum <|> oneOf "_'"
    , opStart       = opLetter henk
    , opLetter      = oneOf ":=\\->/|~.*[]"
    , reservedOpNames = ["::","=","\\","->","=>","/\\","\\/"
                        ,"|~|",".",":","*","[]"]
    , reservedNames = [ "case", "data", "letrec", "type"
          , "import", "in", "let", "of", "at", "Int"
          ]
                , caseSensitive  = True
    }
