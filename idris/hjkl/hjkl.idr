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

%name SizedString str

abstract
data Lines : Vect k Nat -> Type where
  Nil : Lines []
  (::) : SizedString n -> Lines v -> Lines (n :: v)

index : {v : Vect k Nat} -> (i : Fin k) -> Lines v -> SizedString (index i v)
index FZ (x :: xs) = x
index (FS y) (x :: xs) = index y xs

vectSizeVector : Vect k String -> Vect k Nat
vectSizeVector = map length

readFromVect : (v : Vect k String) -> Lines (vectSizeVector v)
readFromVect [] = []
readFromVect (x :: xs) = sizeString x :: readFromVect xs

listSizeVector : (xs : List String) -> Vect (length xs) Nat
listSizeVector xs = vectSizeVector $ fromList xs

readFromList : (xs : List String) -> Lines (listSizeVector xs)
readFromList xs = readFromVect $ fromList xs

writeLinesToList : Lines v -> List String
writeLinesToList [] = []
writeLinesToList ((SizedString' _ s) :: lines) = s :: writeLinesToList lines

data Cursor : Nat -> Type where
  EmptyCursor : Cursor Z
  Cursor' : Fin (S k) -> Cursor (S k)

zeroCursor : (n : Nat) -> Cursor n
zeroCursor Z = EmptyCursor
zeroCursor (S k) = Cursor' FZ

private
boundColumnCursor : Cursor (S k) -> Vect (S k) Nat -> Type
boundColumnCursor (Cursor' fin) nats = Cursor (index fin nats)

abstract
data Buffer : Vect (S k) Nat -> Type where
  Buffer' : {v : Vect (S k) Nat} ->
            Lines v ->
            (rowCursor : Cursor (S k)) ->
            (colCursor : boundColumnCursor rowCursor v) ->
            Buffer v

emptyBuffer : Buffer [Z]
emptyBuffer = Buffer' [sizeString ""] (Cursor' FZ) EmptyCursor

private
zeroRowCursor : (Cursor n -> Buffer v) -> Buffer v
zeroRowCursor {n} partiallyConstructedBuffer = partiallyConstructedBuffer $ zeroCursor n

bufferTypeFromStrings : (xs : List String) -> Type
bufferTypeFromStrings [] = Buffer [Z]
bufferTypeFromStrings (x :: xs) = Buffer $ listSizeVector (x :: xs)

readIntoBuffer : (xs: List String) -> bufferTypeFromStrings xs
readIntoBuffer [] = emptyBuffer
readIntoBuffer (x :: xs) = zeroRowCursor $ Buffer' (readFromList $ x :: xs) (Cursor' FZ)

writeToList : Buffer v -> List String
writeToList (Buffer' l _ _) = writeLinesToList l

data Move x = Backward x | Forward x

data ByCharacter = ByCharacter' Nat

data ByLine = ByLine' Nat

moveCursorBackward : Fin (S k) -> Nat -> Fin (S k)
moveCursorBackward FZ _ = FZ
moveCursorBackward (FS x) Z = (FS x)
moveCursorBackward (FS x) (S k) = moveCursorBackward (weaken x) k

moveCursorForward : Fin (S k) -> Nat -> Fin (S k)
moveCursorForward x Z = x
moveCursorForward x (S k) =
  case strengthen x of
       Left _ => x
       Right x' => FS x'

moveByCharInLine : Cursor n -> Move ByCharacter -> Cursor n 
moveByCharInLine EmptyCursor y = EmptyCursor
moveByCharInLine (Cursor' x) (Backward (ByCharacter' k)) = Cursor' $ moveCursorBackward x k
moveByCharInLine (Cursor' x) (Forward (ByCharacter' k)) = Cursor' $ moveCursorForward x k

partial
moveByChar : {v : Vect (S k) Nat} -> Buffer v -> Move ByCharacter -> Buffer v
moveByChar (Buffer' lines rowCursor columnCursor) movement = ?hole
  --let columnCursor' = moveByCharInLine columnCursor movement
  --in Buffer' lines rowCursor columnCursor'

h : Nat -> Move ByCharacter
h x = Backward (ByCharacter' x)

j : Nat -> Move ByLine
j x = Forward (ByLine' x)

l : Nat -> Move ByCharacter
l x = Forward (ByCharacter' x)

k : Nat -> Move ByLine
k x = Backward (ByLine' x)

