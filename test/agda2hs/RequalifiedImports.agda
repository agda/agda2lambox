open import Haskell.Prelude

import QualifiedImportee as C

requalBar : Integer
requalBar = C.foo

import QualifiedImportee as A

requalBaz : Integer
requalBaz = 2 A.!# 2

requalFooable : A.Foo
requalFooable = C.doTheFoo

import QualifiedImportee as B

requalDefaultBar : B.Foo
requalDefaultBar = B.defaultFoo

import Haskell.Prelude as Pre
import OtherImportee as Other
open import OtherImportee using (OtherFoo)

T = Pre.Integer

otherFoo : OtherFoo
otherFoo = Other.MkFoo -- this qualification is not retained

test : Integer
test = requalBar + requalBaz
     + (case requalFooable of λ where C.MkFoo → 0)
     + (case requalDefaultBar of λ where B.MkFoo → 0)
     - (case otherFoo of λ where Other.MkFoo → 1)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
