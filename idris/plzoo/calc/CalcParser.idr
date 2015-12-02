module CalcParser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

import Syntax

%access private

plusSign : Parser ()
plusSign = token "+"

minusSign : Parser ()
minusSign = token "-"

timesSign : Parser ()
timesSign = token "*"

digit : Parser Char
digit = satisfy isDigit

digits : Parser String
digits = pack <$> some digit

numeral : Parser Expr
numeral = lexeme $ Numeral <$> (cast <$> digits)

binaryExpr : (Expr -> Expr -> Expr) -> Parser () -> Parser Expr -> Parser Expr
binaryExpr ctr pchar pexpr = (ctr <$> (pexpr <* pchar)) <*>| pexpr

times : Parser Expr -> Parser Expr
times = binaryExpr Times timesSign

minus : Parser Expr -> Parser Expr
minus = binaryExpr Minus minusSign

plus : Parser Expr -> Parser Expr
plus = binaryExpr Plus plusSign

negate : Parser Expr -> Parser Expr
negate p = Negate <$> (minusSign *> p)

public
exprParser : Parser Expr
exprParser = times numeral <|>|
             plus numeral <|>|
             minus numeral <|>|
             negate numeral <|>|
             numeral

