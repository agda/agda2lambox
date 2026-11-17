open import Haskell.Prelude

data A : Type where
  MkA : Nat → A

data B : Type where
  MkB : Nat → B

postulate A≡B : A ≡ B

coerceExample : B
coerceExample = coerce A≡B (MkA 5)

test : B
test = coerceExample
{-# COMPILE AGDA2LAMBOX test #-}
