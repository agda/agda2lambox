-- ** type-level uses of functions that are not type schemes
--
-- regression test: only type schemes are declared as types, so a type that applies
-- any other function compiles to its reduct (or 𝕋), not to a type that is never declared;
-- `_∘_` has a single clause of variables, but its type is no arity
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

_∘_ : ∀ {a b c} {A : Set a} {B : Set b} {C : Set c} → (B → C) → (A → B) → A → C
(f ∘ g) x = f (g x)

record Wrap (n : Nat) : Set where
  constructor wrap
  field
    tag    : Nat
    unwrap : ((λ _ → Nat) ∘ suc) n
open Wrap

mkWrap : (n : Nat) → Wrap n
mkWrap n = wrap n 40

test : Nat
test = let w = mkWrap 2 in tag w + unwrap w
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
