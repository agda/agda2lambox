{-# OPTIONS --erase-record-parameters #-}

module _ where

open import Haskell.Prelude

record ClassA (a : Type) : Type where
  field
    myA : a

open ClassA ⦃ ... ⦄ public

record ClassB (b : Type) : Type where
  field
    overlap ⦃ super ⦄ : ClassA b

myB : {{ClassB b}} → b
myB = myA

instance
  iClassANat : ClassA Nat
  iClassANat .myA = 42

  iClassBNat : ClassB Nat
  iClassBNat = record {}

test : Nat
test = myB
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
