open import Agda.Builtin.Nat using (Nat; _+_)

-- ** no unused type parameters
data Either (A B : Set) : Set where
  Left  : A → Either A B
  Right : B → Either A B

fromEitherAB : ∀ {A B : Set} → A → Either A B → A
fromEitherAB _    (Left  a) = a
fromEitherAB defA (Right _) = defA

fromEither : ∀ {A : Set} → A → ∀ {B : Set} → Either A B → A
fromEither _    (Left  a) = a
fromEither defA (Right _) = defA

-- ** unused type parameters present
data OnlyLeft (A B : Set) : Set where
  Left : A → OnlyLeft A B

private variable A B C : Set

fromOnlyLeft : OnlyLeft A B → A
fromOnlyLeft (Left a) = a

fromOnlyLeft2 : OnlyLeft (OnlyLeft A B) C → A
fromOnlyLeft2 (Left (Left a)) = a

record OnlyLeftR (A B : Set) : Set where
  field left : A
open OnlyLeftR public

fromOnlyLeftR : OnlyLeftR A B → A
fromOnlyLeftR r = r .left

fromOnlyLeftR2 : OnlyLeftR (OnlyLeftR A B) C → A
fromOnlyLeftR2 r = r .left .left

open import Agda.Builtin.Nat

test : Nat
test = fromEitherAB {B = Nat} 42 (Left 0)
     + fromEither 0 {B = Nat} (Right 42)
     + fromOnlyLeft {B = Nat} (Left 0)
     + fromOnlyLeft2 {B = Nat} {C = Nat} (Left (Left 0))
     + fromOnlyLeftR2 {B = Nat} {C = Nat} (record { left = record { left = 42 } })
{-# COMPILE AGDA2LAMBOX test #-}
