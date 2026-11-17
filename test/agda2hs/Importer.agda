open import Haskell.Prelude

open import Importee
open import OtherImportee using (MkFoo)

bar : Integer
bar = foo

anotherBar : Integer
anotherBar = anotherFoo

baz : Integer
baz = 21 !# 21

mkFoo : Foo
mkFoo = MkFoo -- This is MkFoo from Importee, NOT from OtherImportee!!

fooable : Foo
fooable = doTheFoo

defaultBar : Foo
defaultBar = defaultFoo

testFoldMap : List Nat → List Nat
testFoldMap = foldMap _∷_ []

testFoldr : List Nat → Nat
testFoldr = foldr (λ _ x → x) 0

test : Integer
test = bar + anotherBar + baz
     + (case mkFoo of λ where MkFoo → 1)
     + (case fooable of λ where MkFoo → 1)
     + (case defaultBar of λ where MkFoo → 1)
     + (if testFoldMap (1 ∷ 2 ∷ []) == 1 ∷ 2 ∷ [] then 2 else 0)
     + (if testFoldr (1 ∷ 2 ∷ []) == 0 then 1 else 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 754
_ = refl
