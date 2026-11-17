open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

{-# TERMINATING #-}
pred : Nat → Nat
pred = λ where
  zero    → zero
  (suc n) → n

{-# TERMINATING #-}
predWhere : Nat → Nat
predWhere n = go n
  where
  go : Nat → Nat
  go = λ where
    zero    → zero
    (suc m) → m

test : Nat
test = pred 3 + predWhere 5
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 6
_ = refl
