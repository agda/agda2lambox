open import Haskell.Prelude

instance
  favoriteNumber : Integer
  favoriteNumber = 42

get : {{Integer}} → Integer
get {{x}} = x

test : Integer
test = get
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
