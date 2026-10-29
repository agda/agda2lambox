open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

test : Nat
test = 1 + 2
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 3
_ = refl
