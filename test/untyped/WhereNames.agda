open import Agda.Builtin.Nat

infixr 5 _∷_

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

length : List Nat → Nat
length = go where go = λ where
  []       → 0
  (_ ∷ xs) → suc (go xs)

double : Nat → Nat
double = go where go = λ n → n + n

test : Nat
test = double (length (1 ∷ 2 ∷ 3 ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}
