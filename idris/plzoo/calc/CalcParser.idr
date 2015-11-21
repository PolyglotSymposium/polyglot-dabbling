module CalcParser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

plus : Parser Char
plus = char '+'

minus : Parser Char
minus = char '-'

times : Parser Char
times = char '*'

lparen : Parser Char
lparen = char '('

rparen : Parser Char
rparen = char ')'

digit : Parser Char
digit = satisfy isDigit

numeral : Parser String -- TODO probably need to make this an integer
numeral = lexeme $ pack <$> many digit
