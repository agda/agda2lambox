open import Agda.Builtin.Nat using () renaming (Nat to ℕ)

record X : Set where
  field id : ℕ
open X public

exX : X
exX .id = 42

record Y : Set where
  field id : ℕ
open Y public

exY : Y
exY = λ where .id → 42

idX : X → ℕ
idX = id

idY : Y → ℕ
idY = id

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t {} | {} | {} | {} ", module_path!(),
    X·id(X{id: 42}),
    Y·id(Y{id: 42}),
    idX(exX()),
    idY(exY()),
  );
}
#-}

open import Agda.Builtin.Nat

test : Nat
test = X.id (record {id = 42})
     + Y.id (record {id = 42})
     + idX exX
     + idY exY
{-# COMPILE AGDA2LAMBOX test #-}
