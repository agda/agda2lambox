open import Haskell.Prelude

module _ (@0 n : Bool) where

record R : Type where
  no-eta-equality
  field
    fld : Integer
open R public

foo : R → Integer
foo x = fld x

test : Integer
test = foo (record { fld = 42 })
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
