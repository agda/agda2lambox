module Issue94 where

open import Haskell.Prelude

thing : List a → List a
thing xs = aux xs
  where
    aux : List a → List a
    aux xs = xs

test : Nat
test = sum (thing (40 ∷ 2 ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
