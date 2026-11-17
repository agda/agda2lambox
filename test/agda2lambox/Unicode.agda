module Unicode where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

s⇒o₁ : List Nat → List Nat
s⇒o₁ _ = 1 ∷ []

test : List Nat
test = s⇒o₁ (1 ∷ 2 ∷ 3 ∷ [])
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 1 ∷ []
_ = refl
