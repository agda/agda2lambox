module Issue73 where

open import Haskell.Prim using (Type)

record ImplicitField (a : Type) : Type where
    field
        aField : a
        @0 {anImplicitField} : a
open ImplicitField public

open import Haskell.Prim using (Nat; _≡_; refl)

test : Nat
test = aField (record { aField = 42 ; anImplicitField = 0 })
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
