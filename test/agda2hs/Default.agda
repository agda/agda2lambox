open import Haskell.Prelude

record HasDefault (a : Type) : Type where
  field
    theDefault : a
open HasDefault {{...}}

instance
  defaultBool : HasDefault Bool
  defaultBool .theDefault = False

test : Bool
test = theDefault
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ False
_ = refl
