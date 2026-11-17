module Issue65 where

open import Haskell.Prelude

yeet : (c : Bool) → (@0 {{c ≡ True}} → a) → (@0 {{c ≡ False}} → a) → a
yeet False x y = y {{refl}}
yeet True  x y = x {{refl}}

-- The branches start with instance lambdas that should be dropped.
xx : Integer
xx = yeet True 1 2

test : Integer
test = xx + yeet False 1 41
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
