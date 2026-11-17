open import Haskell.Prelude hiding (max)
open import Haskell.Law.Equality hiding (subst)

subst : {p : @0 a → Type} {@0 m n : a} → @0 m ≡ n → p m → p n
subst refl t = t

max : Nat → Nat → Nat
max zero    n       = n
max (suc m) zero    = suc m
max (suc m) (suc n) = suc (max m n)

data Tree (a : Type) : (@0 height : Nat) → Type where
  Tip : Tree a 0
  Bin : ∀ {@0 l r} (x : a) → Tree a l → Tree a r → Tree a (suc (max l r))

@0 max-comm : (@0 l r : Nat) → max l r ≡ max r l
max-comm zero zero = refl
max-comm zero (suc r) = refl
max-comm (suc l) zero = refl
max-comm (suc l) (suc r) = cong suc (max-comm l r)

mirror : ∀ {@0 h} → Tree a h → Tree a h
mirror Tip = Tip
mirror {a = a} (Bin {l} {r} x lt rt) =
  subst {p = Tree a} (cong suc (max-comm r l)) (Bin x (mirror rt) (mirror lt))

test : Nat
test = max 2 12 + leftmost (mirror (Bin 10 (Bin 5 Tip Tip) (Bin 20 Tip Tip)))
  where
    leftmost : ∀ {@0 h} → Tree Nat h → Nat
    leftmost Tip         = 0
    leftmost (Bin x l _) = x + leftmost l
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
