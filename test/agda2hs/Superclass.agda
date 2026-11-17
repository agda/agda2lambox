{-# OPTIONS --erase-record-parameters #-}
open import Haskell.Prelude

record Super (a : Type) : Type where
  field
    myFun : a → a
open Super {{...}}

record Sub (a : Type) : Type where
  field
    overlap {{super}} : Super a
open Sub {{...}}

foo : {{Sub a}} → a → a
foo = myFun ∘ myFun

-- Trying if diamonds are fine
record Sub2 (a : Type) : Type where
  field
    overlap {{super}} : Super a
open Sub2 {{...}}

record Subber (a : Type) : Type where
  field
    overlap {{super}} : Sub a
    overlap {{super2}} : Sub2 a
open Subber {{...}}

bar : {{Subber a}} → a → a
bar = myFun ∘ id

instance
  iSuperInt : Super Integer
  iSuperInt .myFun = 1 +_

instance
  iSubInt : Sub Integer
  iSubInt = record{}

-- Defining a subclass of an existing class from Prelude

record DiscreteOrd (a : Type) : Type where
  field
    overlap {{super}} : Ord a
open DiscreteOrd {{...}}

instance
  iDiscreteOrdBool : DiscreteOrd Bool
  iDiscreteOrdBool = record {}

baz : {{DiscreteOrd a}} → a → Bool
baz x = x < x

usebaz : Bool
usebaz = baz True

test : Integer
test = foo 0 + bar 0 + (if usebaz then 0 else 39)
  where
    instance
      iSub2Int : Sub2 Integer
      iSub2Int = record {}

      iSubberInt : Subber Integer
      iSubberInt = record {}
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
