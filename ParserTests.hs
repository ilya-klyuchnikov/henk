module ParserTests where

import Test.HUnit
import ParseError
import Parser

parse1 :: Parser a -> Source -> Either ParseError a
parse1 p s = parse p "" s


testParsing :: (Eq a, Show a) => String -> Parser a -> Source -> a -> Test
testParsing msg parser input expected =
  TestCase (assertEqual msg (Right expected) (parse1 parser input))

test1 = testParsing "test1" (char 'a') "a" 'a'
test2 = testParsing "test2" (char 'a' <|> char 'b') "b" 'a'

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
