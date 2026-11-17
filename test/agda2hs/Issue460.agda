open import Haskell.Prelude

prepend : Bool → List Integer → List Integer
prepend b xs = (if b then 1 ∷ [] else []) ++ xs

test : List Integer
test = prepend True (prepend False (41 ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 1 ∷ 41 ∷ []
_ = refl
