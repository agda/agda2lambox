module Inlining where

open import Haskell.Prelude

Alias : Type
Alias = Bool

aliased : Alias
aliased = True

record Wrap (a : Type) : Type where
  constructor Wrapped
  field
    unwrap : a
open Wrap public

mapWrap : (f : a → b) → Wrap a → Wrap b
mapWrap f (Wrapped x) = Wrapped (f x)

mapWrap2 : (f : a → b → c) → Wrap a → Wrap b → Wrap c
mapWrap2 f (Wrapped x) (Wrapped y) = Wrapped (f x y)

test1 : Wrap Integer → Wrap Integer
test1 x = mapWrap (1 +_) x

test2 : Wrap Integer → Wrap Integer → Wrap Integer
test2 x y = mapWrap2 _+_ x y

test3 : Wrap Integer → Wrap Integer → Wrap Integer
test3 x = mapWrap2 _+_ x

test4 : Wrap Integer → Wrap Integer → Wrap Integer
test4 = mapWrap2 _+_

test : Integer
test = unwrap (test2 (test1 (Wrapped 20)) (Wrapped (if aliased then 21 else 0)))
     + unwrap (test3 (Wrapped 1) (Wrapped 2))
     + unwrap (test4 (Wrapped 3) (Wrapped 4))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 52
_ = refl
