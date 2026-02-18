open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (Nat)

data Maybe {a} (A : Set a) : Set a where
  Nothing : Maybe A
  Just : A → Maybe A

private variable a : Level; A : Set a

idMaybe : Maybe A → Maybe A
idMaybe x = x

m0 m1 : Maybe Nat
m0 = Nothing
m1 = Just 6

fromMaybeNat : Maybe Nat → Nat
fromMaybeNat Nothing  = 6
fromMaybeNat (Just n) = n

fromMaybe : ∀ {a}{A : Set a} → A → Maybe A → A
fromMaybe def Nothing  = def
fromMaybe _   (Just x) = x

open import Agda.Builtin.Nat

toNat = fromMaybe 0

test : Nat
test = toNat (idMaybe(Just 6)) + toNat (idMaybe m1)
     + fromMaybeNat Nothing + fromMaybeNat m0 + fromMaybeNat m1
     + fromMaybe 6 m0 + fromMaybe 0 m1
{-# COMPILE AGDA2LAMBOX test #-}
