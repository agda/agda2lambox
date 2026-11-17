module ValueIndex where

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data _≤_ (m : Nat) : Nat → Set where
  done : m ≤ m
  step : {n : Nat} → m ≤ n → m ≤ suc n

steps : {m n : Nat} → m ≤ n → Nat
steps done     = 0
steps (step p) = suc (steps p)

twice : Nat → Nat
twice n = n + n

le : (n : Nat) → twice n ≤ suc (suc (twice n))
le n = step (step done)

test : Nat
test = steps (le 3)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 2
_ = refl
