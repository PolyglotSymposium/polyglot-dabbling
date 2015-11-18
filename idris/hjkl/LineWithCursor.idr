module LineWithCursor

import Data.Fin

data NonEmptyString : Nat -> Type where
  NonEmptyString' : (line: String) -> {n : Nat} -> { auto nonEmpty: length line = S n } -> NonEmptyString n

data Line : Nat -> Type where
  EmptyLine : Line Z
  Line' : NonEmptyString ll -> Fin (S ll) -> Line (S ll)

data Move = Backward Nat | Forward Nat

--total
moveByChar : Line ll -> Move -> Line ll
moveByChar EmptyLine _ = EmptyLine
moveByChar (Line' line FZ) (Backward _) =
  Line' line FZ
moveByChar line (Backward Z) =
  line
moveByChar (Line' line (FS cursor')) (Backward (S move')) =
  moveByChar (Line' line (weaken cursor')) (Backward move')
moveByChar line (Forward Z) =
  line
moveByChar (Line' line cursor) (Forward (S move')) =
  case strengthen cursor of
       Left _ => Line' line cursor
       Right cursor' => moveByChar (Line' line (FS cursor')) (Forward move')
