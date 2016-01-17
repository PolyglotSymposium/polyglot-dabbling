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

ensureReduces : List a -> List a -> List a
ensureReduces (x :: xs) ys = if length xs < length ys then xs else ys
ensureReduces _ _ = []

%assert_total
parseZeroOrMore : (List Char -> ParseResult (a, List Char)) -> List Char -> (List a, List Char)
parseZeroOrMore parser [] = ([], [])
parseZeroOrMore parser text =
  case parser text of
    Left _ => ([], text)
    Right (firstValue, inputAfterFirstParse) =>
      let
        toParse = ensureReduces text inputAfterFirstParse
        (subsequentValues, remainingInput) = parseZeroOrMore parser toParse
        values = firstValue::subsequentValues
      in
        (values, remainingInput)

many : Parser a -> Parser (List a)
many (MkParser parser) = MkParser (Right . parseZeroOrMore parser)

many1 : Parser a -> Parser (List a)
many1 (MkParser parser) = MkParser doParse where
  doParse text = do
    (firstValue, inputAfterFirstParse) <- parser text
    let toParse = ensureReduces text inputAfterFirstParse
    let (subsequentValues, remainingInput) = parseZeroOrMore parser toParse
    let values = firstValue::subsequentValues
    pure (values, remainingInput)

opt : Parser a -> Parser (Maybe a)
opt (MkParser parser) =
  let
    some = Just <$> MkParser parser
    none = pure Nothing
  in
    some `orElse` none

(<*) : Parser a -> Parser a -> Parser a
(<*) p1 = map fst . andThen p1

(*>) : Parser a -> Parser a -> Parser a
(*>) p1 = map snd . andThen p1

between : Parser a -> Parser a -> Parser a -> Parser a
between p1 p2 p3 = p1 *> p2 <* p3

parse : Parser a -> List Char -> ParseResult (a, List Char)
parse (MkParser parser) text = parser text
