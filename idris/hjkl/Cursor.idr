module Cursor

import Data.Fin
import Data.Vect

%default total
%access public

data Move x = Backward x | Forward x

data ByCharacter = ByCharacter' Nat

data ByLine = ByLine' Nat

data Cursor : Nat -> Type where
  EmptyCursor : Cursor Z
  Cursor' : Fin (S k) -> Cursor (S k)

zeroCursor : (n : Nat) -> Cursor n
zeroCursor Z = EmptyCursor
zeroCursor (S k) = Cursor' FZ

cursorToNat : Cursor n -> Nat
cursorToNat EmptyCursor = Z
cursorToNat (Cursor' x) = finToNat x

columnsInLine : Cursor (S k) -> Vect (S k) Nat -> Nat
columnsInLine (Cursor' fin) lines = index fin lines

data BufferCursor : Vect (S nRows) Nat -> Type where
  BufferCursor' : {size : Vect (S nRows) Nat} ->
            (rowCursor : Cursor (S nRows)) ->
            (colCursor : Cursor (columnsInLine rowCursor size)) ->
            (prevColumn : Nat) ->
            BufferCursor size

moveCursorBackward : Fin (S k) -> Nat -> Fin (S k)
moveCursorBackward FZ _ = FZ
moveCursorBackward (FS x) Z = (FS x)
moveCursorBackward (FS x) (S k) = moveCursorBackward (weaken x) k

moveCursorForward : Fin (S k) -> Nat -> Fin (S k)
moveCursorForward x Z = x
moveCursorForward x (S k) =
  case strengthen x of
       Left _ => x
       Right x' => moveCursorForward (FS x') k

moveByCharInLine : Cursor n -> Move ByCharacter -> Cursor n 
moveByCharInLine EmptyCursor y = EmptyCursor
moveByCharInLine (Cursor' x) (Backward (ByCharacter' k)) = Cursor' $ moveCursorBackward x k
moveByCharInLine (Cursor' x) (Forward (ByCharacter' k)) = Cursor' $ moveCursorForward x k
