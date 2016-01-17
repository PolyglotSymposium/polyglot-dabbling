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

andThen : Parser a -> Parser b -> Parser (a, b)
andThen (MkParser parser1) (MkParser parser2) = MkParser doParse where
  doParse text = do
    (result1, rest1) <- parser1 text
    (result2, rest2) <- parser2 rest1
    return ((result1, result2), rest2)

%name Parser parser

instance Functor Parser where
  map f (MkParser parse) = MkParser doParse where
    doParse text = map mapFirst $ parse text where
      mapFirst (x, y) = (f x, y)

instance Applicative Parser where
  pure x = MkParser $ Right . MkPair x
  (<*>) fn parser = map (\(f, x) => f x) (fn `andThen` parser)

lift2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 fn2 parser1 parser2 = (pure fn2) <*> parser1 <*> parser2

instance Show Lexeme where
  show LeftParenthesis = "("
  show RightParenthesis = ")"
  show (Id x) = x
  show (LiteralInt x) = show x
  show (LiteralString x) = show x

lexChars : Parser (List Lexeme)
lexChars = ?todo

orElse : Parser a -> Parser a -> Parser a
orElse (MkParser parser1) (MkParser parser2) = MkParser doParse where
  doParse text =
    let
      result1 = parser1 text
    in
      if isRight result1 then result1 else parser2 text

choice : Vect (S k) (Parser a) -> Parser a
choice (x :: xs) = foldr orElse x xs

char : Char -> Parser Char
char c = MkParser char' where
  char' : List Char -> ParseResult (Char, List Char)
  char' [] = Left UnexpectedEndOfString
  char' (x :: xs) = if c == x
     then Right (c, xs)
     else Left $ Unrecognized x

anyOf : Vect (S k) Char -> Parser Char
anyOf = choice . map char

digit : Parser Char
digit = anyOf $ fromList ['0'..'9']

sequence : List (Parser a) -> Parser (List a)
sequence [] = pure []
sequence (x :: xs) = lift2 (::) x $ sequence xs

string : String -> Parser (List Char)
string text = sequence $ map char $ unpack text

many : Parser a -> Parser (List a)
many parser = ?hmmm where
  parseZeroOrMore (MkParser parser) text =
    case parser text of
      Left _ => (List.Nil, text)
      Right (firstValue, inputAfterFirstParse) =>
        let
          (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
          values = firstValue::subsequentValues
        in
          (values, remainingInput)

-- TODO: Delete me, for fun only
threeDigits : Parser (List Char)
threeDigits =
  let
    parser = (digit `andThen` digit) `andThen` digit
    transformer = \((a, b), c) => [a, b, c]
  in
    map transformer parser
