module Issue257 where

open import Haskell.Prelude

record Wrap : Type where
  field int : Integer

open import Agda.Builtin.Int using (pos)

test : Nat
test = case Wrap.int (record { int = 42 }) of λ where
  (pos n) → n
  _       → 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
