module Tags

{- The purpose of this code is to answer a question @mjgpy3 asked me.
 - It is a variation on the conceptual code he sent me.
 - It is not meant to be good code or have any practical use other than didactice.
 - -}

import Data.Vect

data ProgrammingLanguageTag : Vect n String -> Type where
  Tag : {langs : Vect n String} -> (s : String) -> {auto prf : elem s langs = True} -> ProgrammingLanguageTag langs

PossibleTags : Vect 3 String
PossibleTags = ["clojure", "idris", "haskell"]

tag1 : ProgrammingLanguageTag PossibleTags
tag1 = Tag "clojure"

tag2 : ProgrammingLanguageTag PossibleTags
tag2 = Tag "idris"

-- Won't compile
--tag3 : ProgrammingLanguageTag PossibleTags
--tag3 = Tag "python"
