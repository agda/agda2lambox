open import Haskell.Prelude

foo : Integer
foo = 43

_!#_ : Integer → Integer → Integer
x !# y = x - y

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

private
  test : Integer
  test = (case doTheFoo of λ where MkFoo → foo) !# (case defaultFoo of λ where MkFoo → 1)
  {-# COMPILE AGDA2LAMBOX test #-}

  _ : test ≡ 42
  _ = refl
