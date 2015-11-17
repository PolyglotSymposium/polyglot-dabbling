module hjkl

import Data.Fin
import Data.Vect

data Cursor : Nat -> Type where
  OnEmpty : Cursor Z
  Cursor' : Fin n -> Cursor n

data Buffer : Type where
  Buffer' : (lines : Vect rows String) ->
            (rowCursor : Fin rows) ->
            { columns : Nat } ->
            { auto prf : length (index rowCursor lines) = columns } ->
            (columnCursor : Cursor columns) ->
            Buffer

data Move x = Backward x | Forward x

data ByCharacter = ByCharacter' Nat

data ByLine = ByLine' Nat

h : Nat -> Move ByCharacter
h x = Backward (ByCharacter' x)

j : Nat -> Move ByLine
j x = Forward (ByLine' x)

l : Nat -> Move ByCharacter
l x = Forward (ByCharacter' x)

k : Nat -> Move ByLine
k x = Backward (ByLine' x)
