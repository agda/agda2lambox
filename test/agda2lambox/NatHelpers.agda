open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

-- Agda's simplifier would turn these into the primitives PQuot and PRem
div : Nat → Nat → Nat
div m zero    = zero
div m (suc n) = div-helper 0 n m n

mod : Nat → Nat → Nat
mod m zero    = m
mod m (suc n) = mod-helper 0 n m n

test : Nat
test = div 17 5 + mod 17 5
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 5
_ = refl
