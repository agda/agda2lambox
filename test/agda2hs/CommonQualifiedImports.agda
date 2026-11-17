import Haskell.Prelude as Common
import Importee as Common
  using (foo)
import Importee as Common
  using (anotherFoo)

foos : Common.Integer
foos = Common.foo Common.+ Common.anotherFoo

open import Haskell.Prelude using (_≡_; refl; fromNat)

test : Common.Integer
test = foos
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 708
_ = refl
