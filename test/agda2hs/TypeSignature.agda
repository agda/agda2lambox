module TypeSignature where

open import Agda.Builtin.Nat
open import Haskell.Prim

five : Nat
five = the (Nat -> Nat) id 5

test : Nat
test = five
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 5
_ = refl
