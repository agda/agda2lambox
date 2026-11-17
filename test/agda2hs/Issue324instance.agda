open import Haskell.Prelude

instance
  eqBoolFun : Eq (Bool → Bool)
  eqBoolFun ._==_ x y = x True == y True && x False == y False

private
  test : Nat
  test = (if id == id then 21 else 0) + (if not == id then 0 else 21)
  {-# COMPILE AGDA2LAMBOX test #-}

  _ : test ≡ 42
  _ = refl
