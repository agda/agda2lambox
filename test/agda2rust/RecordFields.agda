open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (suc; _+_) renaming (Nat to ℕ)

private variable
  ℓ : Level
  A : Set ℓ

data Wrap {ℓ} (A : Set ℓ) : Set ℓ where
  mk : A → Wrap A

unmk : Wrap A → A
unmk (mk a) = a

record Point : Set where
  field slot      : Wrap ℕ
        blockHash : ℕ
open Point public

matchPoint : Point → ℕ
matchPoint record {slot = mk n} = n

exPoint : Point
exPoint .slot      = mk 42
exPoint .blockHash = 0

Hash = ℕ

record Header : Set where
  field  slotNo blockNo : ℕ
         blockHash      : Hash
         prev nodeId    : ℕ
open Header public

matchHeader : Header → ℕ
matchHeader record {slotNo = n} = n

exHeader : Header
exHeader = λ where
  .slotNo → 42
  .blockNo → 0
  .blockHash → 0
  .prev → 0
  .nodeId → 0

data Tx : Set where
  inc dec : Tx

pred : ℕ → ℕ
pred 0 = 0
pred (suc n) = n

open import Agda.Builtin.Nat

test : Nat
test = unmk (exPoint .slot)
     + matchPoint exPoint
     + slotNo (record
                 { slotNo = 42
                 ; blockNo = 0
                 ; blockHash = 0
                 ; prev = 0
                 ; nodeId = 0
                 })
     + matchHeader exHeader
{-# COMPILE AGDA2LAMBOX test #-}
