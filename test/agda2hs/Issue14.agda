module Issue14 where

open import Haskell.Prelude

-- Wrong name for shadowed lambda
constid : a → b → b
constid x = λ x → x

sectionTest₁ : Nat → Nat → Nat
sectionTest₁ n = _+ n

sectionTest₂ : Nat → Nat → Nat
sectionTest₂ section = _+ section

test : Nat
test = constid True 2 + sectionTest₁ 10 20 + sectionTest₂ 5 5
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
