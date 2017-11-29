module TokenDef where

import Parser

data TokenDefinition  = TokenDef
               { commentLine    :: String
               , identStart     :: Parser Char
               , identLetter    :: Parser Char
               , opStart        :: Parser Char
               , opLetter       :: Parser Char
               , reservedNames  :: [String]
               , reservedOpNames:: [String]
               }

henk  = TokenDef
    { commentLine  = "--"
    , identStart  = letter
    , identLetter  = alphaNum <|> oneOf "_'"
    , opStart       = opLetter henk
    , opLetter      = oneOf ":=\\->/|~.*[]"
    , reservedOpNames = ["::","=","\\","->","=>","/\\","\\/"
                        ,"|~|",".",":","*","[]"]
    , reservedNames = [ "case", "data", "letrec", "type"
          , "import", "in", "let", "of", "at", "Int"
          ]
    }

tokenDef    = henk
