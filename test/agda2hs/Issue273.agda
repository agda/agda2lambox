module Issue273 where

open import Haskell.Prelude

test₀ : Integer × Integer → Integer
test₀ = λ x → snd $ x

mySnd : Integer × Integer → Integer
mySnd x = snd x

test2 : Integer × Integer → Integer
test2 = λ x → mySnd $ x

test3 : Integer × Integer → Integer
test3 = λ x → snd x

test4 : Integer × Integer → Integer
test4 = λ x → mySnd x

test5 : Integer × Integer → Integer → Integer
test5 = λ x _ → snd $ x

test6 : Integer → Integer
test6 = _- (1 + 1)

test7 : Integer → Integer
test7 = _+ (1 - 1)

test : Integer
test = test₀ (1 , 2) + test2 (1 , 2) + test3 (1 , 2) + test4 (1 , 2) + test5 (1 , 2) 0
     + test6 24 + test7 10
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
