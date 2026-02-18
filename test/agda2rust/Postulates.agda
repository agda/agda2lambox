open import Agda.Builtin.Nat using (Nat; zero; suc)

postulate TODO : ∀ {A : Set} → A

max : Nat → Nat
max = TODO

testMax : Nat
testMax with 0
... | zero  = 42
... | suc _ = max 42

getTestMax : Nat
getTestMax = testMax

postulate Key : Set
-- {-# COMPILE AGDA2RUST Key = u64 #-}

idKey : Key → Key
idKey k = k

postulate getDefaultKey : Key
-- {-# COMPILE AGDA2RUST getDefaultKey = getDefaultKey #-}
-- {-# FOREIGN AGDA2RUST
-- fn getDefaultKey() -> u64 {
--   42
-- }
-- #-}

testGetKey : Key
testGetKey = getDefaultKey

postulate defaultKey : Key
-- {-# COMPILE AGDA2RUST defaultKey const = 42 #-}

testKey : Key
testKey = defaultKey

postulate hash : ∀ {A : Set} → A → Key
-- {-# COMPILE AGDA2RUST hash = idHash #-}
-- {-# FOREIGN AGDA2RUST
-- use std::hash::{DefaultHasher, Hash, Hasher};

-- fn idHash<A: Hash>(x: i32) -> u64 {
--   let mut s = DefaultHasher::new();
--   x.hash(&mut s);
--   s.finish()
-- }
-- #-}

testHash : Key
testHash = hash testMax

open import Agda.Builtin.Nat

toNat : Key → Nat
toNat _ = 42

test : Nat
test = testMax
     + getTestMax
{-# COMPILE AGDA2LAMBOX test #-}
