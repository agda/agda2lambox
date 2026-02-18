open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; _+_)

sum : List Nat → Nat
sum [] = 0
sum (x ∷ xs) = x + sum xs

pattern [_⨾_] x y = x ∷ y ∷ []

testSum : Nat
testSum = sum [ 3 ⨾ 7 ]

private variable A B C : Set

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

zipWith : (A → B → C) → List A → List B → List C
zipWith f []       _        = []
zipWith f _        []       = []
zipWith f (a ∷ as) (b ∷ bs) = f a b ∷ zipWith f as bs

open import Agda.Builtin.Nat

test : Nat
test = 2 + testSum
         + sum (8 ∷ 2 ∷ [])
         + sum (map suc (7 ∷ 1 ∷ []))
         + sum (zipWith _+_ (5 ∷ []) (5 ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}
