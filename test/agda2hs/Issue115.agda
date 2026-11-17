open import Haskell.Prim using (Type)

record Pointed (a : Type) : Type where
  field
    it : a
open Pointed {{...}} public

data A : Type where A1 : A

instance
  iPointedA : Pointed A
  iPointedA .it = A1

data Delay (a : Type) : Type where
  Later : Delay a → Delay a
  Now : a → Delay a

open import Haskell.Prim using (_≡_; refl)

test : Delay A
test = Later λ where → Now it
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ Later (Now A1)
_ = refl
