-- ** dearging through lets
--
-- regression test: MetaRocq's dearging mask of a constant counts the dead λs under its
-- leading lets, where Agda's usage analysis stops: `k` is `let x = fsuc □ (fzero □) in λ _ → x`,
-- so its unapplied use must be η-expanded, else Peregrine dearges nothing
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data Fin : Nat → Set where
  fzero : {n : Nat} → Fin (suc n)
  fsuc  : {n : Nat} → Fin n → Fin (suc n)

toℕ : {n : Nat} → Fin n → Nat
toℕ fzero    = 0
toℕ (fsuc i) = suc (toℕ i)

const : {A B : Set} → A → B → A
const x = λ _ → x

k : Nat → Fin 3
k = const (fsuc fzero)

record Fun : Set where
  constructor mk
  field app : Nat → Fin 3

run : Fun → Nat
run f = 41 + toℕ (Fun.app f 0)

test : Nat
test = run (mk k)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
