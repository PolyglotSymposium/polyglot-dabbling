module hjkl

import Data.Fin
import Data.Vect

data NonEmptyString : Nat -> Type where
  NonEmptyString' : (line: String) -> {n : Nat} -> { auto nonEmpty: length line = S n } -> NonEmptyString n

total
length : NonEmptyString n -> Nat
length (NonEmptyString' line) = length line

total
lengthOfNonEmptyStringIsPlusOne : (nes : NonEmptyString n) -> length nes = S n
lengthOfNonEmptyStringIsPlusOne (NonEmptyString' line {n=n} {nonEmpty=prf}) = prf

data Line : Nat -> Type where
  EmptyLine : Line Z
  Line' : NonEmptyString ll -> Fin (S ll) -> Line (S ll)

data Move x = Backward x | Forward x

data ByCharacter = ByCharacter' Nat

h : Nat -> Move ByCharacter
h x = Backward (ByCharacter' x)

l : Nat -> Move ByCharacter
l x = Forward (ByCharacter' x)

--total
moveByChar : Line ll -> Move ByCharacter -> Line ll
moveByChar EmptyLine _ = EmptyLine
moveByChar (Line' line FZ) (Backward _) =
  Line' line FZ
moveByChar line (Backward (ByCharacter' Z)) =
  line
moveByChar (Line' line (FS cursor')) (Backward (ByCharacter' (S move'))) =
  moveByChar (Line' line (weaken cursor')) (Backward (ByCharacter' move'))
moveByChar line (Forward (ByCharacter' Z)) =
  line
moveByChar (Line' line cursor) (Forward (ByCharacter' (S move'))) =
  case strengthen cursor of
       Left _ => Line' line cursor
       Right cursor' => moveByChar (Line' line (FS cursor')) (Forward (ByCharacter' move'))
