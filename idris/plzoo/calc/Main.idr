module Main

import Lightyear.Strings

import Syntax
import CalcParser
import Eval

startup : String
startup = "Calculator. Press CTRL-C to quit."

calculate : String -> String
calculate input =
  case parse exprParser input of
       Left error => error
       Right expr => show $ eval expr

main : IO ()
main = do
  putStrLn startup
  repl "> " calculate

