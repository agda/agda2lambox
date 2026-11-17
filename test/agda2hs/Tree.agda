open import Haskell.Prelude

data _≤_ : Nat → Nat → Type where
  instance
    zero-≤ : ∀ {@0 n} → zero ≤ n
    suc-≤ : ∀ {@0 m n} → @0 {{m ≤ n}} → suc m ≤ suc n

data Tree (@0 l u : Nat) : Type where
  Leaf  : @0 {{l ≤ u}} → Tree l u
  Node  : (x : Nat) → Tree l x → Tree x u → Tree l u

test : Nat
test = total tree + (case leq of λ where suc-≤ → 2)
  where
    tree : Tree 0 50
    tree = Node 20 Leaf (Node 20 Leaf Leaf)

    leq : 1 ≤ 2
    leq = suc-≤

    total : ∀ {@0 l u} → Tree l u → Nat
    total Leaf           = 0
    total (Node x tl tr) = x + total tl + total tr
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
