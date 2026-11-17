open import Haskell.Prim using (Type)

module Issue264 (@0 name : Type) where

data Term : @0 Type → Type where
  Dummy : Term name

reduce : Term name → Term name
reduce v = go v
  where
    go : Term name → Term name
    go v = v

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Equality

test : Nat
test = go (reduce Dummy)
  where
    go : Term name → Nat
    go Dummy = 42
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
