open import Haskell.Prelude

mutual

  data Map (k : Type) (a : Type) : Type where
    Bin : (sz : Nat) → (kx : k) → (x : a)
          → (l : Map k a) → (r : Map k a)
          → {{@0 szVal : sz ≡ (size l) + (size r) + 1}}
          → Map k a
    Tip : Map k a

  size : {k a : Type} → Map k a → Nat
  size Tip = 0
  size (Bin sz _ _ _ _) = sz

test : Nat
test = size bin
     + (case bin of λ where
          (Bin _ k x _ _) → k + x
          Tip             → 0)
  where
    bin : Map Nat Nat
    bin = Bin 1 1 40 Tip Tip {{refl}}
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
