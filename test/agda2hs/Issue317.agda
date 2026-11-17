open import Haskell.Prelude

record D : Type where
  no-eta-equality
  constructor C
  field unC : Integer
open D public

test₀ : D → D
test₀ d = C ∘ unC $ d

test : Integer
test = unC (test₀ (C 42))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
