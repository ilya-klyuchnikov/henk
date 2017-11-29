module ParserTests where

import Test.HUnit
import ParseError
import Parser
import ParseToken
import HenkParser

parse1 :: Parser a -> Source -> Either ParseError a
parse1 p s = parse p "" s


testParsing :: (Eq a, Show a) => String -> Parser a -> Source -> a -> Test
testParsing msg parser input expected =
  TestCase (assertEqual msg (Right expected) (parse1 parser input))

test1 = testParsing "test1" (char 'a') "a" 'a'
test21 = testParsing "test2" (char 'a' <|> char 'b') "a" 'a'
test22 = testParsing "test2" (char 'a' <|> char 'b') "b" 'b'

-- p :: Parser Char
test3 = testParsing
          "test3"
          (do {x <- char 'a'; y <- char 'b'; return (x, y)})
          "ab"
          ('a', 'b')


-- using Applicative - there should be a theorem showing exactly the same behavior
-- it means, that (>>=) in our case can be defined via <**>
test4 = testParsing
          "test4"
          ((return (,)) <**> (char 'a') <**> (char 'b'))
          "ab"
          ('a', 'b')

parsePair :: Parser a -> Parser b -> Parser (a, b)
parsePair p1 p2 = (do {x <- p1; y <- p2; return (x, y)})

example01 =
  parse1
  ((parsePair (char 'a') (char 'b')) <|> (parsePair (char 'c') (char 'd')))
  "xxx"

brackets' :: Parser [Char]
brackets' = do {open <- char '(';
               inner <- brackets';
               close <- char ')';
               return ([open] ++ inner ++ [close])} <|> (return "")

example02 = parse1 (try brackets') "(((a)))"

allTests = TestList [test1, test21, test22, test3, test4]

dataBoolDecl = "data Bool : * = { True : Bool ;  False : Bool }"

exampleA = parse1 program dataBoolDecl

exampleB = parse1 expr "Bool"

exampleC = parse1 expr "of"
