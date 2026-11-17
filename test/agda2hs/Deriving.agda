open import Haskell.Prelude

data Planet : Type where
  Mercury : Planet
  Venus   : Planet
  Earth   : Planet
  Mars    : Planet
  Jupiter : Planet
  Saturn  : Planet
  Uranus  : Planet
  Neptune : Planet
  Pluto   : Planet

instance
  iPlanetEq : Eq Planet
  iPlanetEq ._==_ Mercury Mercury = True
  iPlanetEq ._==_ Venus   Venus   = True
  iPlanetEq ._==_ Earth   Earth   = True
  iPlanetEq ._==_ Mars    Mars    = True
  iPlanetEq ._==_ Jupiter Jupiter = True
  iPlanetEq ._==_ Saturn  Saturn  = True
  iPlanetEq ._==_ Uranus  Uranus  = True
  iPlanetEq ._==_ Neptune Neptune = True
  iPlanetEq ._==_ Pluto   Pluto   = True
  iPlanetEq ._==_ _       _       = False

postulate instance iPlanetOrd : Ord Planet

postulate instance iPlanetShow : Show Planet

record Clazz (a : Type) : Type where
  field
    foo : a → Integer
    bar : a → Bool

open Clazz ⦃...⦄ public

postulate instance iPlanetClazz : Clazz Planet

data Optional (a : Type) : Type where
  Of    : a → Optional a
  Empty : Optional a

postulate instance iOptionalEq : ⦃ Eq a ⦄ → Eq (Optional a)

data Duo (a b : Type) : Type where
  MkDuo : (a × b) → Duo a b

instance
  iDuoEq : ⦃ Eq a ⦄ → ⦃ Eq b ⦄ → Eq (Duo a b)
  iDuoEq ._==_ (MkDuo d1) (MkDuo d2) = fst d1 == fst d2 && snd d1 == snd d2

test : Nat
test = (if Earth == Earth then 10 else 0)
     + (if Mars == Venus then 0 else 10)
     + (if MkDuo (Earth , Pluto) == MkDuo (Earth , Pluto) then 10 else 0)
     + (case Of 2 of λ where
          (Of n) → n
          Empty  → 0)
     + (if bar ⦃ clazz ⦄ Neptune then 10 else 0)
  where
    clazz : Clazz Planet
    clazz = record { foo = λ _ → 0 ; bar = λ _ → True }
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
