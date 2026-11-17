module TypeOperatorExport where

open import Haskell.Prim

_<_ : Type -> Type -> Type
a < b = a

data _***_ {i j : Level} (a : Type i) (b : Type j) : Type (i ⊔ j) where
  _:*:_ : a -> b -> a *** b
open _***_ public

_&&&_ : Bool -> Bool -> Bool
False &&& _  = False
_     &&& b2 = b2

private
  test : Nat
  test = case (False &&& True) :*: (True &&& True) of λ where
    (x :*: y) → addNat (if x then 1 else 40) (if y then 2 else 0)
  {-# COMPILE AGDA2LAMBOX test #-}

  _ : test ≡ 42
  _ = refl
