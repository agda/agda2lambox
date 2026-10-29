id it : ∀ {A : Set} → A → A
id x = x
it = id

k drop : ∀ {A B : Set} → A → B → A
k x _ = x
drop = k

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

test : Nat
test = id 42
     + it 42
     + k 42 0
     + drop 42 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 168
_ = refl
