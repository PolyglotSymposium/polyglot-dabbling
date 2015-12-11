module Set

%default total
%access public

||| A proof that a value is not in a list
data Outside : a -> List a -> Type where
  NotInEmpty : Outside x []
  NotInHeadOrTail : Eq t => {x, y: t} -> Outside x xs -> x /= y = True -> Outside x (y :: xs)

data Contains : a -> List a -> Type where
  AsHead : Contains x (x::xs)
  InTail : Contains x xs -> Contains x (y::xs)

instance Uninhabited (Contains a []) where
  uninhabited AsHead impossible
  uninhabited (InTail _) impossible

||| A proof that all elements in a list are unique
data AllUnique : List a -> Type where
  UniqueInEmpty : AllUnique []
  UniqueInHeadAndTail : AllUnique xs -> Outside x xs -> AllUnique (x :: xs)

||| A unique list of items
data Set : a -> Type where
  MkSet : (l : List a) -> { auto prf : AllUnique l } -> Set a

data IsSubset : Set a -> Set a -> Type where
  EmptySetIsSubsetOfAllSets : IsSubset (MkSet []) set
  TailIsSubsetAndContainsHead : Eq t => {x : t} ->
                                { auto uniquexs : AllUnique xs} ->
                                { auto uniqueys : AllUnique ys} ->
                                { auto uniquexxs : AllUnique (x :: xs)} ->
                                IsSubset (MkSet xs {prf=uniquexs}) (MkSet ys) ->
                                Contains x ys ->
                                IsSubset (MkSet (x :: xs) {prf=uniquexxs}) (MkSet ys)

Example1 : IsSubset (MkSet [3, 2]) (MkSet [1, 2, 3, 4, 5])
Example1 =
  TailIsSubsetAndContainsHead (TailIsSubsetAndContainsHead EmptySetIsSubsetOfAllSets (InTail AsHead)) (InTail (InTail AsHead))


Example2 : IsSubset (MkSet [3, 2]) (MkSet [1, 3, 4, 5]) -> Void
Example2 (TailIsSubsetAndContainsHead (TailIsSubsetAndContainsHead EmptySetIsSubsetOfAllSets AsHead) _) impossible
Example2 (TailIsSubsetAndContainsHead (TailIsSubsetAndContainsHead EmptySetIsSubsetOfAllSets (InTail AsHead)) _) impossible
Example2 (TailIsSubsetAndContainsHead (TailIsSubsetAndContainsHead EmptySetIsSubsetOfAllSets (InTail (InTail AsHead))) _) impossible
Example2 (TailIsSubsetAndContainsHead (TailIsSubsetAndContainsHead EmptySetIsSubsetOfAllSets (InTail (InTail (InTail AsHead)))) _) impossible
Example2 (TailIsSubsetAndContainsHead (TailIsSubsetAndContainsHead EmptySetIsSubsetOfAllSets (InTail (InTail (InTail (InTail AsHead))))) _) impossible
