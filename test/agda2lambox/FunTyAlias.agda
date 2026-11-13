open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

Endo : Set → Set
Endo A = A → A

incr : Endo Nat
incr = suc

test : Nat
test = incr (incr 40)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
