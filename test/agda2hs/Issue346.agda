data D : Set
data D where  DC : D

record R : Set
record R where constructor RC

f : D → R
f DC = RC

data Dummy : Set where
  Dum : Dummy

open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.Equality

test : Nat
test = r (f DC) + d Dum
  where
    r : R → Nat
    r RC = 40

    d : Dummy → Nat
    d Dum = 2
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
