open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

const : Nat → @0 Nat → Nat
const n _ = n

pred : Nat → Nat
pred = λ where
  zero    → zero
  (suc n) → const n (pred n)

test : Nat
test = pred 3
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 2
_ = refl
