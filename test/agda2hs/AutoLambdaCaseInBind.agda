open import Haskell.Prelude

lcaseInsideBind : Maybe (Maybe a) → Maybe a
lcaseInsideBind mx = do
  x ← mx
  let go : Maybe a → Maybe a
      go = λ where Nothing  → Nothing
                   (Just _) → Nothing
  go x

test : Nat
test = maybe 21 id (lcaseInsideBind (Just (Just 0)))
     + maybe 21 id (lcaseInsideBind (Just Nothing))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
