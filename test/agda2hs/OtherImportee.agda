open import Haskell.Prelude

data OtherFoo : Type where
  MkFoo : OtherFoo

private
  test : Nat
  test = case MkFoo of λ where MkFoo → 42
  {-# COMPILE AGDA2LAMBOX test #-}

  _ : test ≡ 42
  _ = refl
