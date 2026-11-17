open import Haskell.Prim hiding (f)

module _ where

module A where

  data D : Type where
    C : D

  f : D → D
  f C = C

  g : D
  g = h
    where
      h : D
      h = C

open A public

test : Nat
test = case f g of λ where C → 42
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
