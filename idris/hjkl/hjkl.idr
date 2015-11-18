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

writeToList : Lines v -> List String
writeToList [] = []
writeToList ((SizedString' _ s) :: lines) = s :: writeToList lines

data Cursor : Nat -> Type where
  EmptyCursor : Cursor Z
  Cursor' : Fin (S k) -> Cursor (S k)

zeroCursor : (n : Nat) -> Cursor n
zeroCursor Z = EmptyCursor
zeroCursor (S k) = Cursor' FZ

private
boundColumnCursor : Cursor k -> Vect k Nat -> Type
boundColumnCursor EmptyCursor [] = Cursor Z
boundColumnCursor (Cursor' fin) nats = Cursor (index fin nats)

data Buffer : Vect k Nat -> Type where
  Buffer' : {v : Vect rows Nat} ->
            Lines v ->
            (rowCursor : Cursor rows) ->
            (colCursor : boundColumnCursor rowCursor v) ->
            Buffer v

zeroBuffer : Buffer []
zeroBuffer = Buffer' [] EmptyCursor EmptyCursor

emptyBuffer : Buffer [Z]
emptyBuffer = Buffer' [sizeString ""] (Cursor' FZ) EmptyCursor

zeroRowCursor : (Cursor n -> Buffer v) -> Buffer v
zeroRowCursor {n} partiallyConstructedBuffer = partiallyConstructedBuffer $ zeroCursor n

readIntoBuffer : (xs: List String) -> Buffer (listSizeVector xs)
readIntoBuffer [] = zeroBuffer
readIntoBuffer (x :: xs) = zeroRowCursor $ Buffer' (readFromList $ x :: xs) (Cursor' FZ)

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
moveByChar : {v : Vect k Nat} -> Buffer v -> Move ByCharacter -> Buffer v
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

