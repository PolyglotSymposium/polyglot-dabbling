module LineWithCursor

import Data.Fin

data NonEmptyString : Nat -> Type where
  NonEmptyString' : (line: String) -> {n : Nat} -> { auto nonEmpty: length line = S n } -> NonEmptyString n

data Line : Nat -> Type where
  EmptyLine : Line Z
  Line' : NonEmptyString ll -> Fin (S ll) -> Line (S ll)

data Move = Backward Nat | Forward Nat

total
moveCursorBackward : Fin (S k) -> Nat -> Fin (S k)
moveCursorBackward FZ _ = FZ
moveCursorBackward (FS x) Z = (FS x)
moveCursorBackward (FS x) (S k) = moveCursorBackward (weaken x) k

total
moveCursorForward : Fin (S k) -> Nat -> Fin (S k)
moveCursorForward x Z = x
moveCursorForward x (S k) =
  case strengthen x of
       Left _ => x
       Right x' => FS x'

total
moveCursor : Fin (S k) -> Move -> Fin (S k)
moveCursor x (Backward k) = moveCursorBackward x k
moveCursor x (Forward k) = moveCursorForward x k

total
move : Line ll -> Move -> Line ll
move EmptyLine _ = EmptyLine
move (Line' line cursor) movement =
  Line' line $ moveCursor cursor movement
