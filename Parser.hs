{-----------------------------------------------------------
 Daan Leijen (c) 1999-2000, daan@cs.uu.nl

 $version: 23 Feb 2000, release version 0.2$

 Parsec, the Fast Monadic Parser combinator library.
 http://wwww.cs.uu.nl/~daan/parsec.html

 Inspired by:

    Graham Hutton and Erik Meijer:
    Monadic Parser Combinators.
    Technical report NOTTCS-TR-96-4.
    Department of Computer Science, University of Nottingham, 1996.
    http://www.cs.nott.ac.uk/Department/Staff/gmh/monparsing.ps

 and:

    Andrew Partridge, David Wright:
    Predictive parser combinators need four values to report errors.
    Journal of Functional Programming 6(2): 355-364, 1996
-----------------------------------------------------------}

module Parser(
             --operators: label a parser, alternative
               (<?>), (<|>), (Parser.<**>)

             --basic types
             , Parser, parse, parseFromFile

             , ParseError, errorPos, errorMessages
             , SourcePosition, sourceName, sourceLine, sourceColumn
             , SourceName, Source, Line, Column
             , Message(SysUnExpect,UnExpect,Expect,Msg)
             , messageString, messageCompare, messageEq, showErrorMessages

             --general combinators
             , skipMany, skipMany1
             , many, many1
             , sepBy, sepBy1
             , count
             , chainr1
             , option, optional
             , choice, between
             , oneOf, noneOf
             , anySymbol
             , notFollowedBy

             --language dependent character parsers
             , letter, alphaNum, lower, upper, newline
             , digit, hexDigit, octDigit
             , space, spaces
             , char, anyChar
             , string
             , eof

             --primitive
             , satisfy
             , try
             , token --obsolete, use try instead
             , onFail, unexpected
             ) where

import ParseError
import Control.Applicative hiding (many, optional)
import Data.Char


-----------------------------------------------------------
-- Operators:
-- <?>  gives a name to a parser (which is used in error messages)
-- <|>  is the choice operator
-----------------------------------------------------------
infix  0 <?>


(<?>) :: Parser a -> String -> Parser a
p <?> msg           = onFail p msg


-----------------------------------------------------------
-- Character parsers
-----------------------------------------------------------
spaces              = skipMany space       <?> "white space"
space               = satisfy (isSpace)     <?> "space"

newline             = char '\n'             <?> "new-line"

upper               = satisfy (isUpper)     <?> "uppercase letter"
lower               = satisfy (isLower)     <?> "lowercase letter"
alphaNum            = satisfy (isAlphaNum)  <?> "letter or digit"
letter              = satisfy (isAlpha)     <?> "letter"
digit               = satisfy (isDigit)     <?> "digit"
hexDigit            = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit            = satisfy (isOctDigit)  <?> "octal digit"


-- char c              = satisfy (==c)  <?> show [c]
char c              = do { string [c]; return c}  <?> show [c]
anyChar             = anySymbol

-- string :: String -> Parser String
-- string is defined later as a primitive for speed reasons.


-----------------------------------------------------------
-- General parser combinators
-----------------------------------------------------------
noneOf cs           = satisfy (\c -> not (c `elem` cs))
oneOf cs            = satisfy (\c -> c `elem` cs)

anySymbol           = satisfy (const True)


choice :: [Parser a] -> Parser a
choice ps           = foldr (<|>) mzero ps

option :: a -> Parser a -> Parser a
option x p          = p <|> return x

optional :: Parser a -> Parser ()
optional p          = do{ p; return ()} <|> return ()

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p
                    = do{ open; x <- p; close; return x }


skipMany,skipMany1 :: Parser a -> Parser ()
skipMany1 p         = do{ p; skipMany p }
skipMany p          = scan
                    where
                      scan  = do{ p; scan } <|> return ()

many1,many :: Parser a -> Parser [a]
many1 p             = do{ x <- p; xs <- many p; return (x:xs) }

many p              = scan id
                    where
                      scan f    = do{ x <- p
                                    ; scan (\tail -> f (x:tail))
                                    }
                                <|> return (f [])

sepBy1,sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }

count :: Int -> Parser a -> Parser [a]
count n p           | n <= 0    = return []
                    | otherwise = sequence (replicate n p)


chainr p op x       = chainr1 p op <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op        = scan
                    where
                      scan      = do{ x <- p; rest x }

                      rest x    = do{ f <- op
                                    ; y <- scan
                                    ; return (f x y)
                                    }
                                <|> return x


-----------------------------------------------------------
-- Tricky combinators
-----------------------------------------------------------
eof :: Parser ()
eof                 = notFollowedBy anySymbol <?> "end of input"

notFollowedBy :: Parser Char -> Parser ()
notFollowedBy p     = try (do{ c <- p; unexpected (show [c]) }
                           <|> return ()
                          )



-----------------------------------------------------------
-- Parser definition.
-----------------------------------------------------------
data Parser a       = PT (State -> Processed (Reply a))
runP (PT p)         = p

data Processed a    = Consumed a                --input is consumed
                    | Empty !a                  --no input is consumed

data Reply a        = Ok !a !State ParseError   --parsing succeeded with @a@
                    | Error ParseError          --parsing failed

data State          = ST { stateInput :: !Source
                         , statePos   :: !SourcePosition
                         }
type Source         = String



setExpectError msg err  = setErrorMessage (Expect msg) err
sysUnExpectError msg pos= Error (newErrorMessage (SysUnExpect msg) pos)
unknownError state      = newErrorUnknown (statePos state)

-----------------------------------------------------------
-- run a parser
-----------------------------------------------------------
parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromFile p fname
    = do{ input <- readFile fname
        ; return (parse p fname input)
        }

parse :: Parser a -> SourceName -> Source -> Either ParseError a
parse p name input
    = case parserReply (runP p (ST input (initialPos name))) of
        Ok x _ _    -> Right x
        Error err   -> Left err

parserReply result
    = case result of
        Consumed reply -> reply
        Empty reply    -> reply


instance Functor Reply where
  fmap f (Ok x state err) = let fx = f x in seq fx (Ok fx state err)
  fmap f (Error err) = (Error err)

instance Functor Processed where
  fmap f (Consumed x) = Consumed (f x)
  fmap f (Empty x)    = Empty (f x)

-----------------------------------------------------------
-- Functor: fmap
-----------------------------------------------------------
instance Functor Parser where
  fmap f (PT p)
    = PT (\state ->
        case (p state) of
          Consumed reply -> Consumed (mapReply reply)
          Empty    reply -> Empty    (mapReply reply)
      )
    where
      mapReply reply
        = case reply of
            Ok x state err -> let fx = f x
                              in seq fx (Ok fx state err)
            Error err      -> Error err
  -- alternative via functors:
  -- fmap f (PT p) = PT (fmap (fmap f) . p)


-----------------------------------------------------------
-- Monad: return, sequence (>>=) and fail
-----------------------------------------------------------
instance Applicative Parser where
  pure x = PT (\state -> Empty (Ok x state (unknownError state)))
  (<*>) = undefined

infixl 4 <**>
(<**>) :: Parser (a -> b) -> Parser a -> Parser b
pab <**> pa = do {f <- pab; x <- pa; return (f x)}

infixl 4 <***>
(<***>) :: Parser (a -> b) -> Parser a -> Parser b
pab <***> pa = pab <*> pa


instance Monad Parser where
  (PT p) >>= next
    = PT (\state ->
        case (p state) of
          Consumed reply1
            -> case (reply1) of
                 Ok x state1 err1 -> case runP (next x) state1 of
                                       Empty reply2    -> Consumed (mergeErrorReply err1 reply2)
                                       Consumed reply2 -> Consumed reply2
                 Error err1       -> Consumed (Error err1)

          Empty reply1
            -> case (reply1) of
                 Ok x state1 err1 -> case runP (next x) state1 of
                                       Empty reply2    -> Empty (mergeErrorReply err1 reply2)
                                       Consumed reply2 -> Consumed reply2
                 Error err1       -> Empty (Error err1)
      )


mergeErrorReply err1 reply
  = case reply of
      Ok x state err2 -> Ok x state (mergeError err1 err2)
      Error err2      -> Error (mergeError err1 err2)


-----------------------------------------------------------
-- MonadPlus: alternative (mplus) and mzero
-----------------------------------------------------------
instance Alternative Parser where
  empty
    = PT (\state -> Empty (Error (unknownError state)))

  (PT p1) <|> (PT p2)
    = PT (\state ->
        case (p1 state) of
          Empty (Error err) -> case (p2 state) of
                                 Empty reply -> Empty (mergeErrorReply err reply)
                                 consumed    -> consumed
          other             -> other
      )

mzero :: Parser a
mzero = empty

mplus :: Parser a -> Parser a -> Parser a
mplus = (<|>)
--instance MonadPlus Parser

-----------------------------------------------------------
-- Primitive Parsers:
--  try, satisfy, onFail, unexpected and updateState
-----------------------------------------------------------
try :: Parser a -> Parser a
try (PT p)
    = PT (\state@(ST input pos) ->
        case (p state) of
          Consumed (Error err)  -> Empty (Error (setErrorPos pos err))
          Consumed ok           -> Empty ok
          empty                 -> empty
      )

token p --obsolete, use "try" instead
    = try p

satisfy :: (Char -> Bool) -> Parser Char
satisfy test
    = PT (\(ST input pos) ->
        case input of
          (c:cs) | test c    -> let newpos   = updatePos pos c
                                    newstate = ST cs newpos
                                in seq newpos $ seq newstate $
                                   Consumed (Ok c newstate (newErrorUnknown newpos))
                 | otherwise -> Empty (sysUnExpectError (show [c]) pos)
          []     -> Empty (sysUnExpectError "" pos)
      )


onFail :: Parser a -> String -> Parser a
onFail (PT p) msg
    = PT (\state ->
        case (p state) of
          Empty reply
            -> Empty $
               case (reply) of
                 Error err        -> Error (setExpectError msg err)
                 Ok x state1 err  | errorIsUnknown err -> reply
                                  | otherwise -> Ok x state1 (setExpectError msg err)
          other       -> other
      )

unexpected :: String -> Parser a
unexpected msg
    = PT (\state -> Empty (Error (newErrorMessage (UnExpect msg) (statePos state))))


-----------------------------------------------------------
-- Parsers unfolded for speed:
--  string
-----------------------------------------------------------

{- specification of @string@:
string s            = scan s
                    where
                      scan []     = return s
                      scan (c:cs) = do{ char c <?> show s; scan cs }
-}

string :: String -> Parser String
string str
    = PT (\ (ST input pos) -> walkAll input pos) where
        walkAll input pos = walk1 str input where
          ok cs               = let newpos   = updatePosString pos str
                                    newstate = ST cs newpos
                                in seq newpos $ seq newstate $
                                   (Ok str newstate (newErrorUnknown newpos))

          errEof              = Error (setErrorMessage (Expect (show str))
                                       (newErrorMessage (SysUnExpect "") pos))
          errExpect c         = Error (setErrorMessage (Expect (show str))
                                       (newErrorMessage (SysUnExpect (show [c])) pos))

          walk [] cs          = ok cs
          walk xs []          = errEof
          walk (x:xs) (c:cs)  = if x == c then walk xs cs else errExpect c

          walk1 [] cs         = Empty (ok cs)
          walk1 xs []         = Empty (errEof)
          walk1 (x:xs) (c:cs) = if x == c then Consumed (walk xs cs) else Empty (errExpect c)
