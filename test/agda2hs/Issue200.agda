open import Haskell.Prelude

data Void : Type where

test₀ : Maybe Void → Maybe Void
test₀ = λ
    { Nothing → Nothing
    }

test : Nat
test = maybe 42 (λ ()) (test₀ Nothing)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
