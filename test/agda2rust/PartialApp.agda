open import Agda.Builtin.List using (List; []; _∷_)

private variable A B C : Set

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

zipWith : (A → B → C) → List A → List B → List C
zipWith f []       _        = []
zipWith f _        []       = []
zipWith f (a ∷ as) (b ∷ bs) = f a b ∷ zipWith f as bs

open import Agda.Builtin.Nat using (Nat; _+_)

incr : List Nat → List Nat
incr xs = map (_+ 1) xs

incr2 : List Nat → List Nat
incr2 = map (_+ 1)

sum : List Nat → Nat
sum [] = 0
sum (x ∷ xs) = x + sum xs

pattern [_] x = x ∷ []
pattern [_⨾_] x y = x ∷ [ y ]

testPartialAdd : Nat
testPartialAdd = sum (map (_+ 1) [ 20 ⨾ 20 ])

testIncr : Nat
testIncr = sum (incr [ 20 ⨾ 20 ])

testIncr2 : Nat
testIncr2 = sum (incr2 [ 20 ⨾ 20 ])

testPartialAdd2 : Nat
testPartialAdd2 = sum (zipWith _+_ [ 20 ⨾ 20 ] [ 1 ⨾ 1 ])

constNat : Nat → Nat → Nat
constNat x _ = x

testConstNat : Nat
testConstNat = sum (map (constNat 21) [ 1 ⨾ 2 ])

sumWith : (Nat → Nat) → List Nat → Nat
sumWith f xs = sum (map f xs)

testSumWith : Nat
testSumWith = sumWith (_+ 1) [ 40 ⨾ 0 ]

sumWithConst : (Nat → Nat → Nat) → List Nat → Nat
sumWithConst f xs = sum (map (f 21) xs)

testSumWithConst : Nat
testSumWithConst = sumWithConst constNat [ 1 ⨾ 2 ]

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
