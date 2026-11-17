open import Haskell.Prelude

lcaseInsideCaseOf : List a → (Maybe a → Maybe a)
lcaseInsideCaseOf xs = case xs of λ where
  []      → λ where Nothing → Nothing
                    (Just _) → Nothing
  (x ∷ _) → λ where Nothing → Nothing
                    (Just _) → Just x

test : Nat
test = maybe 0 id (lcaseInsideCaseOf (42 ∷ []) (Just 0))
     + maybe 0 id (lcaseInsideCaseOf [] (Just 1))
     + maybe 0 id (lcaseInsideCaseOf (1 ∷ []) Nothing)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
