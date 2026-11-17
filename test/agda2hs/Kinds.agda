module Kinds where

open import Haskell.Prelude

record ReaderT (r : Type) (m : Type → Type) (a  : Type) : Type where
  no-eta-equality
  constructor RdrT
  field runReaderT : r → m a
open ReaderT public

data Kleisli (m : Type → Type) (a b : Type) : Type where
  K : (a → m b) → Kleisli m a b

test : Nat
test = maybe 0 id (runReaderT (RdrT {m = Maybe} λ r → Just (r + 1)) 20)
     + (case K {m = Maybe} {a = Nat} (λ x → Just (x + 1)) of λ where
          (K f) → maybe 0 id (f 20))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
