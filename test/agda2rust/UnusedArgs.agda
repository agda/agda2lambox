open import Agda.Builtin.Nat using (Nat)

constNat : Nat → Nat → Nat
constNat x _ = x

the42 : Nat
the42 = constNat 42 0

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

test : Nat
test = constNat 42 41
     + the42
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 84
_ = refl
