-- Same as `PartialApp`, but for Maybe instead of List.
open import Agda.Builtin.Maybe using (Maybe; nothing; just)

private variable A B C : Set

map : (A → B) → Maybe A → Maybe B
map f nothing  = nothing
map f (just x) = just (f x)

zipWith : (A → B → C) → Maybe A → Maybe B → Maybe C
zipWith f nothing  _        = nothing
zipWith f _        nothing  = nothing
zipWith f (just a) (just b) = just (f a b)

open import Agda.Builtin.Nat using (Nat; _+_)

incr : Maybe Nat → Maybe Nat
incr xs = map (_+ 1) xs

incr2 : Maybe Nat → Maybe Nat
incr2 = map (_+ 1)

sum : Maybe Nat → Nat
sum nothing  = 0
sum (just x) = x

pattern [_] x = just x

testPartialAdd : Nat
testPartialAdd = sum (map (_+ 1) [ 41 ])

testIncr : Nat
testIncr = sum (incr [ 41 ])

testIncr2 : Nat
testIncr2 = sum (incr2 [ 41 ])

testPartialAdd2 : Nat
testPartialAdd2 = sum (zipWith _+_ [ 40 ] [ 2 ])

constNat : Nat → Nat → Nat
constNat x _ = x

testConstNat : Nat
testConstNat = sum (map (constNat 42) [ 1 ])

sumWith : (Nat → Nat) → Maybe Nat → Nat
sumWith f xs = sum (map f xs)

testSumWith : Nat
testSumWith = sumWith (_+ 1) [ 41 ]

sumWithConst : (Nat → Nat → Nat) → Maybe Nat → Nat
sumWithConst f xs = sum (map (f 42) xs)

testSumWithConst : Nat
testSumWithConst = sumWithConst constNat [ 1 ]

record SomeNat : Set where
  constructor `_
  field someNat : Nat

addSomeNat : SomeNat → (Nat → Nat)
addSomeNat (` n) = n +_

testSomeNat : Nat
testSomeNat = addSomeNat (` 40) 2

open import Agda.Builtin.Nat

test : Nat
test
  = constNat 42 41
  + testPartialAdd
  + testIncr
  + testIncr2
  + testPartialAdd2
  + testConstNat
  + testSumWith
  + testSumWithConst
  + testSomeNat
{-# COMPILE AGDA2LAMBOX test #-}
