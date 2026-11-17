open import Haskell.Prelude

module Issue305 (@0 X : Type) where

record Class (a : Type) : Type where
  field
    foo : a → a
open Class {{...}} public

instance
  ClassInt : Class Integer
  ClassInt .foo = _+ 1

instance
  ClassBool : Class Bool
  ClassBool .foo = not

test₀ : Integer
test₀ = foo 41

anotherTest : Integer
anotherTest = test₀

yetAnotherTest : Integer
yetAnotherTest = case Just True of λ where
  Nothing → error "unreachable"
  (Just y) → foo 5

andOneMoreTest : Integer → Integer
andOneMoreTest x = foo 5

record Subclass (a : Type) : Type where
  field
    overlap {{super}} : Class a
    bar : a
open Subclass {{...}} public

instance
  SubclassBool : Subclass Bool
  SubclassBool .super = ClassBool
  SubclassBool .bar = False

test : Integer
test = anotherTest + yetAnotherTest + andOneMoreTest 0 + (if foo (bar {Bool}) then 1 else 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 55
_ = refl
