module CalcParser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

import Syntax

%access private

data Token =
  NumericToken Integer
  | PlusOp
  | MinusOp
  | TimesOp
  | NegateOp

plusSign : Parser Token
plusSign = (\x => PlusOp) <$> token "+"

minusSign : Parser Token
minusSign = (\x => MinusOp) <$> token "-"

timesSign : Parser Token
timesSign = (\x => TimesOp) <$> token "*"

digit : Parser Char
digit = satisfy isDigit

digits : Parser String
digits = pack <$> some digit

numeral : Parser Token
numeral = lexeme $ NumericToken <$> (cast <$> digits)

singleToken : Parser Token
singleToken = plusSign <|>|
              minusSign <|>|
              timesSign <|>|
              numeral

tokenizer : Parser (List Token)
tokenizer = manyTill singleToken endOfLine

