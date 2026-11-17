module HsNumbers where

open import Haskell.Prelude

posNatural : Nat
posNatural = 14

posInteger : Integer
posInteger = 52

negInteger : Integer
negInteger = -1001

natToPos : Nat → Integer
natToPos n = fromNat n

natToNeg : Nat → Integer
natToNeg n = fromNeg n

test : Integer
test = posInteger + negInteger + natToPos posNatural + natToNeg 2
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ -937
_ = refl
