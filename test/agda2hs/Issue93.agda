module Issue93 where

open import Haskell.Prelude

fun : Bool → Bool
fun x = case x of λ where
          True  → False
          False → y
  where
    y : Bool
    y = True

nested : Maybe Bool → Bool
nested x = case x of λ where
           (Just b) → (case y of λ where
                     True →  b
                     False → z)
           Nothing  → y
  where
    y : Bool
    y = True

    z : Bool
    z = case y of λ where
          True → y
          False → True

test : Nat
test = (if fun False then 10 else 0)
     + (if fun True then 0 else 10)
     + (if nested (Just True) then 10 else 0)
     + (if nested (Just False) then 0 else 10)
     + (if nested Nothing then 2 else 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
