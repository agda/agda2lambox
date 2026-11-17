open import Haskell.Prim using (Type)

data MyBool : Type where
    MyTrue : MyBool
    MyFalse : MyBool

rank2ForallUse : (∀ (a : Type) → a → a) → MyBool
rank2ForallUse f = f MyBool MyTrue

module _  (f : ∀ (a : Type) → a → a) where
    rank2Module : MyBool
    rank2Module = f MyBool MyTrue

data Exist : Type₁ where
    MkExist : ∀ (a : Type) → a → Exist

open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.Equality

test : Nat
test = toNat (rank2ForallUse λ _ x → x) + toNat (rank2Module (λ _ x → x))
     + unpack (MkExist MyBool MyTrue)
  where
    unpack : Exist → Nat
    unpack (MkExist _ _) = 0

    toNat : MyBool → Nat
    toNat MyTrue  = 21
    toNat MyFalse = 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
