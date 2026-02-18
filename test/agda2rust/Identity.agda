id : ∀ {A : Set} → A → A
id x = x

id0 : ∀ {@0 A : Set} → A → A
id0 x = x

id⟨_⟩_ : (A : Set) → A → A
id⟨_⟩_ _ x = x

id0⟨_⟩_ : (@0 A : Set) → A → A
id0⟨_⟩_ _ x = x

idH : (A : Set) → {A} → A
idH _ {x} = x

id0H : (@0 A : Set) → {A} → A
id0H _ {x} = x

open import Agda.Builtin.Nat

test : Nat
test = id0H _ {idH _ {id0⟨ _ ⟩ id⟨ _ ⟩ id0 id 42}}
{-# COMPILE AGDA2LAMBOX test #-}
