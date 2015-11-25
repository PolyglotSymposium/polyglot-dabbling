module Cursor

import Data.Fin
import Data.Vect

%default total
%access public

data Move x = Backward x | Forward x

data ByCharacter = ByCharacter' Nat

data ByLine = ByLine' Nat

private
data RowCursor : Nat -> Type where
  EmptyRowCursor : RowCursor Z
  RowCursor' : Fin (S k) -> RowCursor (S k)

private
zeroRowCursor' : (n : Nat) -> RowCursor n
zeroRowCursor' Z = EmptyRowCursor
zeroRowCursor' (S k) = RowCursor' FZ

private
rowCursorToNat : RowCursor n -> Nat
rowCursorToNat EmptyRowCursor = Z
rowCursorToNat (RowCursor' x) = finToNat x

private
rowCursorToMaybeFin : RowCursor n -> Maybe (Fin n)
rowCursorToMaybeFin EmptyRowCursor = Nothing
rowCursorToMaybeFin (RowCursor' x) = Just x

abstract
data Cursor : Vect (S lastRow) Nat -> Type where
  Cursor' : {size : Vect (S lastRow) Nat} ->
            (rowCursor : Fin (S lastRow)) ->
            (colCursor : RowCursor (index rowCursor size)) ->
            (prevColumn : Nat) ->
            Cursor size

emptyBufferCursor : Cursor [Z]
emptyBufferCursor = Cursor' FZ EmptyRowCursor Z

zeroRowCursor : Cursor size
zeroRowCursor {size} = Cursor' FZ (zeroRowCursor' $ index FZ size) Z

currentRowIndex : {size : Vect (S lastRow) Nat} -> Cursor size -> Fin (S lastRow)
currentRowIndex (Cursor' rowIndex _ _) = rowIndex

columnsInLine : Cursor size -> Nat
columnsInLine {size} cursor = index (currentRowIndex cursor) size

currentColumnIndex : (b : Cursor size) -> Maybe (Fin (columnsInLine b))
currentColumnIndex (Cursor' _ colCursor _) = rowCursorToMaybeFin colCursor

private
moveCursorBackward : Fin (S k) -> Nat -> Fin (S k)
moveCursorBackward FZ _ = FZ
moveCursorBackward (FS x) Z = (FS x)
moveCursorBackward (FS x) (S k) = moveCursorBackward (weaken x) k

private
moveCursorForward : Fin (S k) -> Nat -> Fin (S k)
moveCursorForward x Z = x
moveCursorForward x (S k) =
  case strengthen x of
       Left _ => x
       Right x' => moveCursorForward (FS x') k

private
moveByCharInLine : RowCursor n -> Move ByCharacter -> RowCursor n 
moveByCharInLine EmptyRowCursor y = EmptyRowCursor
moveByCharInLine (RowCursor' x) (Backward (ByCharacter' k)) = RowCursor' $ moveCursorBackward x k
moveByCharInLine (RowCursor' x) (Forward (ByCharacter' k)) = RowCursor' $ moveCursorForward x k

moveByChar : Cursor v -> Move ByCharacter -> Cursor v
moveByChar (Cursor' rowCursor columnCursor _) movement =
  let newColCursor = moveByCharInLine columnCursor movement
  in Cursor' rowCursor newColCursor (rowCursorToNat newColCursor)

