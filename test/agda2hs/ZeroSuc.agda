module ZeroSuc where

open import Haskell.Prelude

test1 : Nat
test1 = zero

test2 : Nat → Nat
test2 = suc

data MyNat : Set where
  MyZero : MyNat
  MySuc : MyNat → MyNat

opaque
  test3 : MyNat → Nat
  test3 MyZero = zero
  test3 (MySuc n) = suc (test3 n)

test : Nat
test = test1 + test2 39 + test3 (MySuc (MySuc MyZero))
{-# COMPILE AGDA2LAMBOX test #-}

opaque
  unfolding test3

  _ : test ≡ 42
  _ = refl
