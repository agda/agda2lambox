module EraseType where

open import Haskell.Prelude
open import Haskell.Extra.Erase

testErase : Erase Integer
testErase = Erased 42

testMatch : Erase Integer → Erase Integer
testMatch (Erased x) = Erased (x + 1)

testSingleton : Singleton (get testErase)
testSingleton = sing 42

testSingletonErase : Singleton testErase
testSingletonErase = singErase

testCong : Singleton (1 + get testErase)
testCong = singCong (1 +_) testSingleton

rTail : ∀ {@0 x xs} → Singleton {a = List Integer} (x ∷ xs) → Singleton xs
rTail ys = singTail ys

open import Haskell.Extra.Refinement using (value)

test : Integer
test = none (testMatch testErase) + none (value testSingletonErase)
     + value testSingleton
     + value testCong
     + total (value (rTail (sing (1 ∷ 2 ∷ []))))
  where
    none : Erase Integer → Integer
    none (Erased _) = 0

    total : List Integer → Integer
    total []       = 0
    total (x ∷ xs) = x + total xs
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 87
_ = refl
