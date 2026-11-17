module Fixities where

open import Haskell.Prelude

leftAssoc : Integer → List Integer
leftAssoc n = 2 * n + 1
            ∷ 2 * (n + 1)
            ∷ 1 + n * 2
            ∷ (1 + n) * 2
            ∷ (n + n) + n
            ∷ n + (n + n)
            ∷ []

rightAssoc : List Integer → List Integer
rightAssoc xs = xs ++ xs ++ ((xs ++ xs) ++ xs) ++ xs

nonAssoc : Bool → Bool
nonAssoc b = (b == b) == (b == b)

mixedAssoc : Maybe Integer → (Integer → Maybe Integer) → Maybe Integer
mixedAssoc m f = f =<< (((f =<< m) >>= f) >>= f)

infixl 7 _<+>_
_<+>_ : Integer → Integer → Integer
x <+> y = x + y

infixr 8 _<->_
_<->_ : Integer → Integer → Integer
x <-> y = x - y

test : Integer
test = foldr _+_ 0 (leftAssoc 1)
     + foldr _+_ 0 (rightAssoc (1 ∷ []))
     + (if nonAssoc True then 1 else 0)
     + maybe 0 id (mixedAssoc (Just 1) (λ n → Just (n + 1)))
     + (8 <+> 4 <-> 2)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
