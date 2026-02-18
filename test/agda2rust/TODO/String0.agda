postulate String : Set
{-# BUILTIN STRING String #-}

s42 : String
s42 = "42"

test : String
test = s42
{-# COMPILE AGDA2LAMBOX test #-}
