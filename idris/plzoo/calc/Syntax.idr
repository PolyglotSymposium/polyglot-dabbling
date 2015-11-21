module Syntax

%default total
%access public

data Expr =
  Numeral Integer
  | Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Negate Expr

%name Expr expr, expr1, expr2

private
to_str : Nat -> Expr -> String
to_str n e =
  let (m, str) = rank_and_str e
  in if m < n then "(" ++ str ++ ")" else str where
    rank_and_str : Expr -> (Nat, String)
    rank_and_str expr =
      case expr of
        Numeral n    => (4, show n)
        Negate e     => (3, "-" ++ (to_str 1 e))
        Times e1 e2  => (2, (to_str 2 e1) ++ " * " ++ (to_str 3 e2))
        Plus e1 e2   => (1, (to_str 1 e1) ++ " + " ++ (to_str 2 e2))
        Minus e1 e2  => (1, (to_str 1 e1) ++ " - " ++ (to_str 2 e2))

instance Show Expr where
  show expr = to_str Z expr
