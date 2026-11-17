open import Haskell.Prelude

import Importee

simpqualBar : Integer
simpqualBar = Importee.foo

simpfoo : Importee.Foo
simpfoo = Importee.Foo.MkFoo

import QualifiedImportee as Qually

qualBar : Integer
qualBar = Qually.foo

qualBaz : Integer
qualBaz = 2 Qually.!# 2

qualFooable : Qually.Foo
qualFooable = Qually.doTheFoo

qualDefaultBar : Qually.Foo
qualDefaultBar = Qually.defaultFoo

Foo : Type
Foo = Importee.Foo

test : Integer
test = simpqualBar + qualBar + qualBaz
     + (case simpfoo of λ where Importee.MkFoo → 1)
     + (case qualFooable of λ where Qually.MkFoo → 1)
     + (case qualDefaultBar of λ where Qually.MkFoo → 1)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 88
_ = refl
