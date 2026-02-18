open import Agda.Builtin.Float using (Float)

idFloat : Float → Float
idFloat f = f

test : Float
test = idFloat 4.2
{-# COMPILE AGDA2LAMBOX test #-}
