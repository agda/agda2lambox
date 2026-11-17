open import Haskell.Prelude

module Issue353 where
-- Calling functions between local anonymous modules removed arguments

module FooBar (a : Bool) where
    hello : Bool
    hello = a

module Foo (a : Bool) where
    -- If the name of the current module is a prefix of the called module
    -- they would be interpreted as the same module
    world : Bool
    world = FooBar.hello a

    open FooBar a
    world2 : Bool
    world2 = hello

module _ (a : Bool) (b : Integer) where
    foo : Bool
    foo = not a

module _ (b : Bool) where

    bar : Bool
    bar = foo True 0

    baz : Bool
    baz = bar

    callFromNested : Bool
    callFromNested = nested
      where nested = bar

-- The check is needed both for instance declarations and where-clauses

test : Nat
test = (if FooBar.hello True then 6 else 0)
     + (if Foo.world True then 6 else 0)
     + (if Foo.world2 True then 6 else 0)
     + (if foo False 1 then 6 else 0)
     + (if bar True then 0 else 6)
     + (if baz True then 0 else 6)
     + (if callFromNested True then 0 else 6)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
