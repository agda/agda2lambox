data Newtype (A : Set) : Set where
  mk : A → Newtype A

private variable A : Set

k : Newtype A → Newtype A → A
k (mk a) _ = a

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

test : Nat
test = k (mk 42) (mk 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
