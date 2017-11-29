module TokenDef where

import Parser

data TokenDefinition  = TokenDef
               { commentStart   :: String
               , commentEnd     :: String
               , commentLine    :: String
               , nestedComments :: Bool
               , identStart     :: Parser Char
               , identLetter    :: Parser Char
               , opStart        :: Parser Char
               , opLetter       :: Parser Char
               , reservedNames  :: [String]
               , reservedOpNames:: [String]
               }

henk  = TokenDef
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
    }

tokenDef    = henk
