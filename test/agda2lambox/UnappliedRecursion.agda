open import Agda.Builtin.Equality
open import Agda.Builtin.Nat

app : (Nat → Nat) → Nat → Nat
app f x = f x

{-# TERMINATING #-}
h : Nat → Nat
h zero    = 0
h (suc n) = suc (app h n)

test : Nat
test = h 3
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 3
_ = refl
