open import Haskell.Prelude

record Class (a : Type) : Type where
  field
    foo : a → a
open Class {{...}} public

module M1 (@0 X : Type) where

  instance
    ClassInt : Class Integer
    ClassInt .foo = _+ 1

module M2 (@0 X : Type) where

  open M1 X

  tester : Integer
  tester = foo 41

test : Integer
test = M2.tester ⊤
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
