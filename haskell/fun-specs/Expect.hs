module Expect where

data Modifier = To | NotTo | ToNot
data Matcher a f = Be a | Fulfill f
data Reality = Fail | Success deriving (Show, Eq)

expect val adv matcher = [case adv of
  To -> if successfulMatch matcher val then Success else Fail
  _  -> if successfulMatch matcher val then Fail else Success]

also e1 e2 = e1 ++ e2

successfulMatch matcher = case matcher of
  (Be a) -> (== a)
  (Fulfill f) -> f

main = print $
  expect (21 * 2) To (Be 42) `also`
  expect 42 ToNot (Be 36) `also`
  expect 42 To (Be 42) `also`
  expect 42 To (Fulfill (> 9)) `also`
  expect 42 ToNot (Fulfill (< 9))
