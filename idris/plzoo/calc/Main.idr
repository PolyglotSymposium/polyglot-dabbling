module Main

startup : String
startup = "Calculator. Press CTRL-C to quit."

main : IO ()
main = do
  putStrLn startup
  repl "> " id

