module Lexer

import Data.Vect

%default total

%access public

data Lexeme =
  LeftParenthesis
  | RightParenthesis
  | Id String
  | LiteralInt Int
  | LiteralString String

data ParseError =
  Unrecognized Char
  | UnexpectedEndOfString

ParseResult : Type -> Type
ParseResult = Either ParseError

data Parser : Type -> Type where
  MkParser : (List Char -> ParseResult (a, List Char)) -> Parser a

instance Functor Parser where
  map f (MkParser parse) = MkParser doParse where
    doParse text = map mapFirst $ parse text where
      mapFirst (x, y) = (f x, y)

instance Show Lexeme where
  show LeftParenthesis = "("
  show RightParenthesis = ")"
  show (Id x) = x
  show (LiteralInt x) = show x
  show (LiteralString x) = show x

lexChars : Parser (List Lexeme)
--lexChars [] = Right ([], [])
--lexChars (x :: xs) = ?foo_1

--andThen : Parser a -> Parser b -> Parser (a, b)
--andThen parser1 parser2 = doParse where
--  doParse text = do
--    (result1, rest1) <- parser1 text
--    (result2, rest2) <- parser2 rest1
--    return ((result1, result2), rest2)
--
--orElse : Parser a -> Parser a -> Parser a
--orElse parser1 parser2 = doParse where
--  doParse text =
--    let
--      result1 = parser1 text
--    in
--      if isRight result1 then result1 else parser2 text
--
--choice : Vect (S k) (Parser a) -> Parser a
--choice (x :: xs) = foldr orElse x xs
--
--char : Char -> Parser Char
--char c = char' where
--  char' : Parser Char
--  char' [] = Left UnexpectedEndOfString
--  char' (x :: xs) = if c == x
--     then Right (c, xs)
--     else Left $ Unrecognized x
--
--anyOf : Vect (S k) Char -> Parser Char
--anyOf = choice . map char
--
--lex : String -> List Lexeme
--lex = ?asdf . lexChars . unpack
