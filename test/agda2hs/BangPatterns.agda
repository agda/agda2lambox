open import Haskell.Prelude
open import Haskell.Prim using (ℓ)
open import Haskell.Prim.Strict

strictId : Strict a → a
strictId (! x) = x

myFoldl : (b -> a -> b) -> b -> List a -> b
myFoldl f x0 [] = x0
myFoldl f x0 (x ∷ xs) = myFoldl f (f x0 x) xs

foldl'' : (b -> a -> b) -> Strict b -> List a -> b
foldl'' f (! x0) [] = x0
foldl'' f (! x0) (x ∷ xs) = foldl'' f (! f x0 x) xs

data LazyMaybe (a : Type ℓ) : Type ℓ where
  LazyNothing : LazyMaybe a
  LazyJust    : a → LazyMaybe a

data StrictMaybe (a : Type ℓ) : Type ℓ where
  StrictNothing : StrictMaybe a
  StrictJust    : Strict a → StrictMaybe a

test : Nat
test = strictId (! 0)
     + myFoldl _+_ 0 (10 ∷ 10 ∷ [])
     + foldl'' _+_ (! 0) (10 ∷ 10 ∷ [])
     + (case LazyJust 1 of λ where
          LazyNothing  → 0
          (LazyJust n) → n)
     + (case StrictJust (! 1) of λ where
          StrictNothing      → 0
          (StrictJust (! n)) → n)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
