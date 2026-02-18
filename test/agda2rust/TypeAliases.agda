open import Agda.Builtin.Nat using (Nat; _+_)

ℕ : Set
ℕ = Nat

testAlias : ℕ
testAlias = 42

ℕ→ℕ : Set
ℕ→ℕ = ℕ → ℕ

incr : ℕ→ℕ
incr = _+ 1

testAliasF : ℕ
testAliasF = incr 41

Id : Set → Set
Id A = A

id : ∀ {A : Set} → Id A → Id A
id x = x

Const : Set → Set → Set
Const A B = A

idK : ∀ {A : Set} → Const A A → Const A A
idK x = x

test : Nat
test = testAlias
     + testAliasF
     + id 42
     + idK 42
{-# COMPILE AGDA2LAMBOX test #-}
