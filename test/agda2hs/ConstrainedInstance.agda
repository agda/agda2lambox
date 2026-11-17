open import Haskell.Prelude

data D (a : Type) : Type where
  C : a → D a

instance
  iEqD : {{Eq a}} → Eq (D a)
  iEqD ._==_ (C x) (C y) = x == y

test : Nat
test = (if C {Nat} 1 == C 1 then 20 else 0)
     + (if C {Nat} 1 == C 2 then 0 else 22)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
