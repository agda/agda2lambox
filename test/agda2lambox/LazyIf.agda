module LazyIf where

open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then t else _ = t
if false then _ else e = e
{-# INLINE if_then_else_ #-}

pred : Nat → Nat
pred zero    = zero
pred (suc n) = n

{-# TERMINATING #-}
count : Nat → Nat
count n = if n == 0 then 0 else suc (count (pred n))
-- ^ the inlined `if` binds both branches before the match,
--   which loops forever unless the else-branch is only evaluated when taken.

test : Nat
test = count 5
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 5
_ = refl
