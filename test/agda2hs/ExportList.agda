module ExportList where

open import Haskell.Prelude using ( a; Nat; suc; zero; _+_; List; []; _∷_; _≡_; refl )
open import Haskell.Extra.Refinement using ( ∃; _⟨_⟩ )
open import Haskell.Law.Equality using ( cong )

private variable
  @0 n : Nat

-- Function

publicFun : {a : Set} → a → a
publicFun x = x

inlineFun : {a : Set} → a → a
inlineFun x = x

transparentFun : {a : Set} → a → a
transparentFun x = x

private
  privateFun : {a : Set} → a → a
  privateFun x = x

module M where

  publicFun2 : {a : Set} → a → a
  publicFun2 x = x

  private
    privateFun2 : {a : Set} → a → a
    privateFun2 x = x

private module M2 where

  privateFun3 : {a : Set} → a → a
  privateFun3 x = x

-- Type synonym

PublicTy : Set
PublicTy = Nat

private
  PrivateTy : Set
  PrivateTy = Nat

-- Datatype

data PublicData : Set where
  MkPublicData : PublicData

private
  data PrivateData : Set where
    MkPrivateData : PrivateData

data PublicNewtype : Set where
  MkPublicNewtype : Nat → PublicNewtype

private
  data PrivateNewtype : Set where
    MkPrivateNewtype : Nat → PrivateNewtype

-- Record

record PublicRecord : Set where
  no-eta-equality
  constructor MkPublicRecord
  field publicField : Nat

open PublicRecord public

record UnboxedRecord : Set where
  no-eta-equality
  constructor MkUnboxedRecord
  field unboxedField : Nat

open UnboxedRecord public

record TupleRecord : Set where
  no-eta-equality
  constructor MkTupleRecord
  field
    tupleField1 : Nat
    tupleField2 : Nat

open TupleRecord public

-- Typeclass

record PublicClass : Set where
  field publicMethod : Nat

open PublicClass ⦃ ... ⦄ public

data Vec (a : Set) : @0 Nat → Set where
  []  : Vec a 0
  _∷_ : a → Vec a n → Vec a (suc n)

llength : List a → Nat
llength [] = 0
llength (x ∷ xs) = 1 + llength xs

vlength : Vec a n → ∃ Nat (_≡ n)
vlength [] = 0 ⟨ refl ⟩
vlength (x ∷ xs) =
  let n ⟨ eq ⟩ = vlength xs
  in  (1 + n) ⟨ cong suc eq ⟩

open import Haskell.Prelude using (case_of_)

test : Nat
test = publicFun 1 + inlineFun 1 + transparentFun 1 + privateFun 1
     + M.publicFun2 1 + M2.privateFun3 1
     + (case MkPublicData of λ where MkPublicData → 1)
     + (case MkPrivateData of λ where MkPrivateData → 1)
     + (case MkPublicNewtype 2 of λ where (MkPublicNewtype n) → n)
     + (case MkPrivateNewtype 2 of λ where (MkPrivateNewtype n) → n)
     + publicField (MkPublicRecord 3) + unboxedField (MkUnboxedRecord 3)
     + tupleField1 (MkTupleRecord 4 5) + tupleField2 (MkTupleRecord 4 5)
     + publicMethod ⦃ record { publicMethod = 11 } ⦄
     + llength (1 ∷ 2 ∷ [])
     + (let n ⟨ _ ⟩ = vlength (1 ∷ 2 ∷ []) in n)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
