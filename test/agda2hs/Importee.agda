open import Haskell.Prelude

foo : Integer
foo = 42

_!#_ : Integer → Integer → Integer
x !# y = x + y

data Foo : Type where
  MkFoo : Foo

-- ** base
record Fooable (a : Type) : Type where
  field doTheFoo defaultFoo : a
-- ** defaults
record DefaultFooable (a : Type) : Type where
  field doTheFoo : a

  defaultFoo : a
  defaultFoo = doTheFoo
-- ** export
open Fooable ⦃...⦄ public
-- ** instances
instance
  FF : Fooable Foo
  FF = record {DefaultFooable (λ where .doTheFoo → MkFoo)}
    where open DefaultFooable

open import SecondImportee public

private
  test : Integer
  test = (case defaultFoo of λ where MkFoo → foo) !# anotherFoo
  {-# COMPILE AGDA2LAMBOX test #-}

  _ : test ≡ 708
  _ = refl
