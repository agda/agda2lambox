open import Agda.Builtin.String

demo : String
demo = "Hello, World!"
{-# COMPILE AGDA2LAMBOX demo #-}
