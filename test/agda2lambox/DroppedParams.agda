-- projection-like functions
{-# OPTIONS --projection-like #-}
open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

data Wrap (A : Set) : Set where
  wrap : A → Wrap A

-- drops `A`
unwrap : {A : Set} → Wrap A → A
unwrap (wrap x) = x

module _ (k : Nat) where
  data Tag : Set where
    tag : Nat → Tag

  -- drops the module parameter `k`
  untag : Tag → Nat
  untag (tag n) = n

map : {A B : Set} → (A → B) → List A → List B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

sum : List Nat → Nat
sum []       = 0
sum (x ∷ xs) = x + sum xs

-- on variables, applied and unapplied
total : Wrap Nat → List (Wrap Nat) → Tag 0 → Nat
total w ws t = unwrap w + sum (map unwrap ws) + untag 0 t

test : Nat
test = total (wrap 10) (wrap 12 ∷ []) (tag 20)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
