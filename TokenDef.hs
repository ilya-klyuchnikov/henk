module TokenDef where

import Parser

data TokenDefinition  = TokenDef
               { commentLine    :: String
               , identStart     :: Parser Char
               , identLetter    :: Parser Char
               , reservedNames  :: [String]
               }

henk  = TokenDef
    { commentLine  = "--"
    , identStart  = letter
    , identLetter  = alphaNum <|> oneOf "_'"
    , reservedNames = [ "case", "data", "letrec", "type"
          , "import", "in", "let", "of", "at", "Int"
          ]
    }

tokenDef    = henk
