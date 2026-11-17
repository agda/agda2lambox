{-# OPTIONS --no-projection-like #-}
module ModuleParametersImports where

open import Haskell.Prelude
open import ModuleParameters Bool (λ _ → Nat)

scope : Scope
scope = unbind (Bind True 3 (Bind False 2 Empty))

test : Nat
test = hd scope + thing 28 + stuff 10 scope + hd (more True False (Bind True 10 scope))
  where
    hd : Scope → Nat
    hd Empty        = 0
    hd (Bind _ n _) = n
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
