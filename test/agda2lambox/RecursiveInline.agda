open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

double : Nat → Nat
double zero    = zero
double (suc n) = suc (suc (double n))
{-# INLINE double #-}

test : Nat
test = double 3
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 6
_ = refl
