{-# OPTIONS --level-universe #-}
open import Agda.Builtin.Nat using (Nat)

open import Agda.Primitive
open import Agda.Builtin.Equality

0ℓ : Level
0ℓ = lzero

testLevel : Level → Nat
testLevel _ = 42

test : Nat
test = testLevel 0ℓ
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
