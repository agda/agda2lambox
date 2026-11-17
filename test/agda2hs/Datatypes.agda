open import Haskell.Prim using (Bool; Type)

data Test : Type where
  CTest : Bool -> @0 {Bool} -> Test

getTest : Test → Bool
getTest (CTest b) = b

putTest : Bool → Test → Test
putTest b (CTest _ {b'}) = CTest b {b'}

open import Haskell.Prim using (Nat; True; False; _≡_; refl)

test : Nat
test = toNat (getTest (putTest True (CTest False {False})))
  where
    toNat : Bool → Nat
    toNat True  = 42
    toNat False = 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
