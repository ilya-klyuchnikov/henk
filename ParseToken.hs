-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- $version: 23 Feb 2000, release version 0.2$
-----------------------------------------------------------
module ParseToken where

import Data.Char         (isSpace,digitToInt,isAlpha,toLower,toUpper)
import Data.List         (nub,sort)
import Parser
import TokenDef


-----------------------------------------------------------
-- Bracketing
-----------------------------------------------------------
parens p        = between (symbol "(") (symbol ")") p
braces p        = between (symbol "{") (symbol "}") p
brackets p      = between (symbol "<") (symbol ">") p
squares p       = between (symbol "[") (symbol "]") p

semi            = symbol ";"
comma           = symbol ","
dot             = symbol "."
colon           = symbol ":"

commaSep p      = sepBy p comma
semiSep p       = sepBy p semi

commaSep1 p     = sepBy1 p comma
semiSep1 p      = sepBy1 p semi

-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
-- integers and naturals
natural         = lexeme nat        <?> "natural"
int             = do{ f <- lexeme sign
                    ; n <- nat
                    ; return (f n)
                    }

sign            :: Parser (Integer -> Integer)
sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

nat             = zeroNumber <|> decimal

zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------
reserved name =
    lexeme $ try $
    do{ string name
      ; notFollowedBy (identLetter tokenDef) <?> ("end of " ++ show name)
      }

identifier =
    lexeme $ try $
    do{ name <- ident
      ; if (isReservedName name)
         then unexpected ("reserved word " ++ show name)
         else return name
      }

ident
    = do{ c <- identStart tokenDef
        ; cs <- many (identLetter tokenDef)
        ; return (c:cs)
        }
    <?> "identifier"

isReservedName name
    = isReserved theReservedNames name

isReserved names name
    = scan names
    where
      scan []       = False
      scan (r:rs)   = case (compare r name) of
                        LT  -> scan rs
                        EQ  -> True
                        GT  -> False

theReservedNames = sortedNames
    where
      sortedNames   = sort (reservedNames tokenDef)


-----------------------------------------------------------
-- White space & symbols
-----------------------------------------------------------
-- used a lot to remove whiteSpaces
symbol name = lexeme (string name)

lexeme p
    = do{ x <- p; whiteSpace; return x  }


--whiteSpace
whiteSpace = skipMany (simpleSpace <|> oneLineComment <?> "")

simpleSpace =
    skipMany1 (satisfy isSpace)

oneLineComment =
    do{ try (string (commentLine tokenDef))
      ; skipMany (satisfy (/= '\n'))
      ; return ()
      }
