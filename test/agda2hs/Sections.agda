module Sections where

open import Haskell.Prelude

test₁ : Nat → Nat
test₁ = 5 +_

test₂ : Nat → Nat
test₂ = _+ 5

test₃ : Nat → Nat
test₃ = _+_ 5

test₄ : Nat → Nat
test₄ = λ x → x + 5

test₅ : Nat → Nat
test₅ = λ x → 5 + x -- Agda eta-contracts this before we get to see it

test : Nat
test = test₁ 1 + test₂ 2 + test₃ 3 + test₄ 4 + test₅ 7
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
