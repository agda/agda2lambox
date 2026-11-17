module Issue421 where

open import Haskell.Prelude

record Identity (a : Type) : Type where
  no-eta-equality
  pattern
  constructor MkIdentity
  field runIdentity : a

open Identity public

instance
  iDefaultFunctorIdentity : DefaultFunctor Identity
  iDefaultFunctorIdentity .DefaultFunctor.fmap f (MkIdentity x) = MkIdentity (f x)

  iFunctorIdentity : Functor Identity
  iFunctorIdentity = record {DefaultFunctor iDefaultFunctorIdentity}

  iDefaultApplicativeIdentity : DefaultApplicative Identity
  iDefaultApplicativeIdentity .DefaultApplicative.pure = MkIdentity
  iDefaultApplicativeIdentity .DefaultApplicative._<*>_ (MkIdentity f) (MkIdentity x) = MkIdentity (f x)

  iApplicativeIdentity : Applicative Identity
  iApplicativeIdentity = record {DefaultApplicative iDefaultApplicativeIdentity}

test : Nat
test = runIdentity (fmap suc (MkIdentity 20)) + runIdentity (pure suc <*> MkIdentity 20)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
