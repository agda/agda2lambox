open import Haskell.Prelude

{- Old style using implicit arguments (no longer supported)
data Vec (a : Type) : {n : Nat} → Type where
  Nil : Vec a {0}
  Cons : {n : Nat} → a → Vec a {n} → Vec a {suc n}

mapV : {a b : Type} {n : Nat} (f : a → b) → Vec a {n} → Vec b {n}
mapV f Nil = Nil
mapV f (Cons x xs) = Cons (f x) (mapV f xs)

tailV : {a : Type} {n : Nat} → Vec a {suc n} → Vec a {n}
tailV (Cons x xs) = xs
-}

-- New style using erasure instead of implicit arguments
data Vec (a : Type) : (@0 n : Nat) → Type where
  Nil : Vec a 0
  Cons : {@0 n : Nat} → a → Vec a n → Vec a (suc n)

mapV : {a b : Type} {@0 n : Nat} (f : a → b) → Vec a n → Vec b n
mapV f Nil = Nil
mapV f (Cons x xs) = Cons (f x) (mapV f xs)

tailV : {a : Type} {@0 n : Nat} → Vec a (suc n) → Vec a n
tailV (Cons x xs) = xs

test : Nat
test = case tailV (mapV (_+ 1) (Cons 1 (Cons 41 Nil))) of λ where
  (Cons x _) → x
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
