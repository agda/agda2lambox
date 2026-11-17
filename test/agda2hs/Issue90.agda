module Issue90 where

open import Haskell.Prelude

good : Nat
good = bar
  where
    foo : Nat
    foo = 42

    bar : Nat
    bar = foo

bad : Nat
bad = bar
  where
    bar : Nat
    foo : Nat
    bar = foo
    foo = 42

good2 : Nat
good2 = bar
  where
    foo : Nat
    foo = 42 + x
      where
        x : Nat
        x = 1
    bar : Nat
    bar = foo + x
      where
        x : Nat
        x = 2

bad2 : Nat
bad2 = bar
  where
    bar : Nat
    foo : Nat
    foo = 42 + x
      where
        x : Nat
        x = 1
    bar = foo + x
      where
        x : Nat
        x = 2

test₀ : Bool → Nat
test₀ True = bar
  where
    foo : Nat
    foo = 42 + ted
      where
        nes : Nat
        nes = 1
        ted : Nat
        ted = nes + 1

    bar : Nat
    bar = foo
test₀ False = bar
  where
    bar : Nat
    foo : Nat
    foo = 42 + ted
      where
        ted : Nat
        nes : Nat
        nes = 1
        ted = nes + 1
    bar = foo

test : Nat
test = good + bad + good2 + bad2 + test₀ True + test₀ False
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 262
_ = refl
