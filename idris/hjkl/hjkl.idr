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

data Cursor : Nat -> Type where
  OnEmpty : Cursor Z
  Cursor' : Fin n -> Cursor n

data Lines : Vect k Nat -> Type where
  Nil : Lines []
  (::) : SizedString n -> Lines v -> Lines (n :: v)

bufferStringSize : (s : List String) -> Vect (length s) Nat
bufferStringSize cs = map length (fromList cs)

vectSizeVector : Vect k String -> Vect k Nat
vectSizeVector xs = map length xs

readFromVect : (v : Vect k String) -> Lines (vectSizeVector v)
readFromVect [] = []
readFromVect (x :: xs) = sizeString x :: readFromVect xs

listSizeVector : (xs : List String) -> Vect (length xs) Nat
listSizeVector xs = vectSizeVector (fromList xs)

readFromList : (xs : List String) -> Lines (listSizeVector xs)
readFromList xs = readFromVect (fromList xs)

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
