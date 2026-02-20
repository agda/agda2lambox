data Bool : Set where false true : Bool

record Pair (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B
open Pair

pair : Pair Bool Bool
pair = true , false

test : Bool
test = snd pair
{-# COMPILE AGDA2LAMBOX test #-}
