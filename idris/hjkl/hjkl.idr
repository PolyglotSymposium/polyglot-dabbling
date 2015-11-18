module hjkl

import Data.Fin
import Data.Vect

%default total

%access public

abstract
data SizedString : Nat -> Type where
  SizedString' : (n : Nat) -> (s : String) -> SizedString n

sizeString : (s : String) -> SizedString (length s)
sizeString s = SizedString' (length s) s

data Lines : Vect k Nat -> Type where
  Nil : Lines []
  (::) : SizedString n -> Lines v -> Lines (n :: v)

vectSizeVector : Vect k String -> Vect k Nat
vectSizeVector = map length

readFromVect : (v : Vect k String) -> Lines (vectSizeVector v)
readFromVect [] = []
readFromVect (x :: xs) = sizeString x :: readFromVect xs

listSizeVector : (xs : List String) -> Vect (length xs) Nat
listSizeVector xs = vectSizeVector $ fromList xs

readFromList : (xs : List String) -> Lines (listSizeVector xs)
readFromList xs = readFromVect $ fromList xs

writeToList : Lines v -> List String
writeToList [] = []
writeToList ((SizedString' _ s) :: lines) = s :: writeToList lines

data Cursor : Nat -> Type where
  EmptyLineCursor : Cursor Z
  Cursor' : Fin k -> Cursor k

boundColumnCursor : Cursor k -> Vect k Nat -> Type
boundColumnCursor EmptyLineCursor [] = Cursor Z
boundColumnCursor (Cursor' fin) nats = Cursor (index fin nats)

data Buffer : Type where
  Buffer' : {v : Vect rows Nat} ->
            Lines v ->
            (rowCursor : Cursor rows) ->
            (colCursor : boundColumnCursor rowCursor v) ->
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
