module Eval

import Syntax

%default total
%access public

eval : Expr -> Integer
eval (Numeral x) = x
eval (Plus expr1 expr2) = eval expr1 + eval expr2
eval (Minus expr1 expr2) = eval expr1 - eval expr2
eval (Times expr1 expr2) = eval expr1 * eval expr2
eval (Negate expr1) = -eval expr1

