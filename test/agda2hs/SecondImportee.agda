open import Haskell.Prelude

anotherFoo : Integer
anotherFoo = 666

private
  test : Integer
  test = anotherFoo
  {-# COMPILE AGDA2LAMBOX test #-}

  _ : test ≡ 666
  _ = refl
